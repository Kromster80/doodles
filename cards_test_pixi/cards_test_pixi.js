"use strict";

// Global resource, meh
let
    cardBack = {name: 'back', url: 'https://deckofcardsapi.com/static/img/back.png'},
    cards = [],
    menuState = 0;

const CARD_WIDTH = 226 * 0.5;
const CARD_HEIGHT = 314 * 0.5;

// Do once on startup
generateListOfCards();
function generateListOfCards() {
    const ranks = ['2', '3', '4', '5', '6', '7', '8', '9', '0', 'J', 'Q', 'K'];
    const suits = ['C', 'D', 'H', 'S'];

    for (let i in ranks) {
        for (let k in suits) {
            cards.push({
                name: ranks[i] + suits[k],
                url: 'https://deckofcardsapi.com/static/img/' + ranks[i] + suits[k] + '.png'
            });
        }
    }
}

const app = new PIXI.Application({ backgroundColor: 0x206020 });
document.body.appendChild(app.view);

app.loader
    .add(cardBack)
    .add(cards)
    .load(onAssetsLoaded);


// onAssetsLoaded handler builds the example.
function onAssetsLoaded() {

    loadTextures();
    function loadTextures() {
        cardBack.texture = PIXI.Texture.from(cardBack.name);
        for (let i in cards) {
            cards[i].texture = PIXI.Texture.from(cards[i].name);
        }
    }

    const cardsContainer = createTable();
    function createTable() {
        let cc = new PIXI.Container();
        cc.y = 0;
        cc.x = 0;
        app.stage.addChild(cc);
        return cc;
    }

    const cardThrow = createCard(cardBack.texture);
    cardThrow.sprite.alpha = 0;
    cardThrow.sprite.interactive = true;
    cardThrow.sprite.on('mousedown', (event) => {
        console.log('Tapped');
        cardThrowCatch();
    });

    function createCard(texture) {
        let c = {};
        c.sprite = new PIXI.Sprite(texture);
        c.sprite.pivot.x = texture.width / 2;
        c.sprite.pivot.y = texture.height / 2;
        c.sprite.scale.x = CARD_WIDTH / texture.width;
        c.sprite.scale.y = CARD_HEIGHT / texture.height;
        c.sprite.x = app.screen.width / 2;
        c.sprite.y = app.screen.height / 2;
        cardsContainer.addChild(c.sprite);
        return c;
    }

    const popupText = createPlayText();
    popupText.addListener('pointerdown', () => {
        toggleState();
    });

    function createPlayText() {
        const style = new PIXI.TextStyle({
            fontFamily: 'Arial',
            fontSize: 36,
            fontStyle: 'italic',
            fontWeight: 'bold',
            fill: ['#fff', '#f40'], // gradient
            stroke: '#4a1850',
            strokeThickness: 5,
            dropShadow: true,
            dropShadowColor: '#000000',
            dropShadowBlur: 4,
            dropShadowAngle: Math.PI / 6,
            dropShadowDistance: 6,
            wordWrap: true,
            wordWrapWidth: 440,
        });

        let newText = new PIXI.Text('Start the game!', style);
        newText.x = Math.round((app.screen.width - newText.width) / 2);
        newText.y = Math.round((app.screen.height - newText.height) / 2);

        newText.interactive = true;
        newText.buttonMode = true;
        newText.addListener('pointerover', () => {
            newText.style.fill = ['#fff', '#f84'];
        });
        newText.addListener('pointerout', () => {
            newText.style.fill = ['#fff', '#f40'];
        });
        app.stage.addChild(newText);

        return newText;
    }

    let currentCard;

    // Start right away
    toggleState();

    // Function to start playing.
    function toggleState() {
        if (menuState === 0) {
            // Start the game!
            popupText.interactive = false;
            tweenTo(popupText, 'alpha', 0, 500, easeSquareRoot(), null, gameStart);
            menuState = 1;
        } else
        if (menuState === 1) {
            // Game over!
            popupText.text = 'Game over!';
            popupText.interactive = true;
            tweenTo(popupText, 'alpha', 1.0, 500, easeSquareRoot(), null, null);
            menuState = 2;
        } else
        if (menuState === 2) {
            popupText.text = 'Start the game!';
            menuState = 0;
        }
    }

    function gameStart() {
        console.log('Game starts');

        currentCard = 0;
        cardThrowNew();
    }

    function cardThrowNew() {
        console.log('Throwing card ' + currentCard);

        cardThrow.sprite.alpha = 1.0;
        cardThrow.sprite.interactive = true;
        cardThrow.sprite.x = app.screen.width + CARD_WIDTH / 2;
        cardThrow.sprite.y = lerp(CARD_HEIGHT/2, app.screen.height - CARD_HEIGHT/2, Math.random());

        const tgtY = lerp(CARD_HEIGHT/2, app.screen.height - CARD_HEIGHT/2, Math.random());
        const tgtRotation = lerp(-8, 8, Math.random());
        const time = 3000 - currentCard * 50;
        cardThrow.tweenX = tweenTo(cardThrow.sprite, 'x', -CARD_WIDTH / 2, time, easeLinear(), null, cardThrowComplete);
        cardThrow.tweenY = tweenTo(cardThrow.sprite, 'y', tgtY, time, easeLinear(), null,null);
        cardThrow.tweenR = tweenTo(cardThrow.sprite, 'rotation', tgtRotation, time, backout(0.5), null, null);
    }

    function cardThrowComplete() {
        console.log('Throw ended');

        if (currentCard < 3) {
            currentCard++;
            cardThrowNew();
        } else {
            console.log('Game ended');
            // Freeze the game (clear all pending animations)
            tweening.splice(0, tweening.length);
            toggleState();
        }
    }

    function cardThrowCatch() {
        // Stop any card tweens
        tweenStop(cardThrow.tweenX);
        tweenStop(cardThrow.tweenY);
        tweenStop(cardThrow.tweenR);
        cardThrow.sprite.alpha = 0;
        cardThrow.sprite.interactive = false;

        // Flip the card
        let c = createCard(cards[currentCard].texture);
        c.sprite.x = cardThrow.sprite.x;
        c.sprite.y = cardThrow.sprite.y;
        c.sprite.rotation = cardThrow.sprite.rotation;

        // Make sure the thrown card is on top
        cardsContainer.swapChildren(cardThrow.sprite, c.sprite);

        // Tween card to hand

        // Throw next card
        cardThrowComplete();
    }
}

// Very simple tweening utility function. This should be replaced with a proper tweening library in a real product.
const tweening = [];
function tweenTo(object, property, target, time, easing, onchange, oncomplete) {
    const tween = {
        object,
        property,
        propertyBeginValue: object[property],
        target,
        easing,
        time,
        change: onchange,
        complete: oncomplete,
        start: Date.now(),
    };

    tweening.push(tween);
    return tween;
}
function tweenStop(tween) {
    tweening.splice(tweening.indexOf(tween), 1);
}
// Listen for animate update.
app.ticker.add((delta) => {
    const now = Date.now();
    const remove = [];
    for (let i = 0; i < tweening.length; i++) {
        const t = tweening[i];
        const phase = Math.min(1, (now - t.start) / t.time);

        t.object[t.property] = lerp(t.propertyBeginValue, t.target, t.easing(phase));
        if (t.change) t.change(t);
        if (phase === 1) {
            t.object[t.property] = t.target;
            if (t.complete) t.complete(t);
            remove.push(t);
        }
    }
    for (let i = 0; i < remove.length; i++) {
        tweening.splice(tweening.indexOf(remove[i]), 1);
    }
});

// Basic lerp funtion.
function lerp(a1, a2, t) {
    return a1 * (1 - t) + a2 * t;
}

// Backout function from tweenjs.
// https://github.com/CreateJS/TweenJS/blob/master/src/tweenjs/Ease.js
function easeLinear() {
    return (t) => (t);
}
function easeSquareRoot() {
    return (t) => (Math.sqrt(t));
}

function backout(amount) {
    return (t) => (Math.sqrt(t));
}