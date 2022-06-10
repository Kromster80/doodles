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

    for (const rank of ranks) {
        for (const suit of suits) {
            cards.push({
                name: rank + suit,
                url: 'https://deckofcardsapi.com/static/img/' + rank + suit + '.png'
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
        for (const card of cards) {
            card.texture = PIXI.Texture.from(card.name);
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

    const cardThrow = createCardSprite(cardBack.texture);
    cardThrow.alpha = 0;
    cardThrow.interactive = true;
    cardThrow.on('mousedown', (event) => {
        console.log('Tapped');
        cardThrowCatch();
    });

    function createCardSprite(texture) {
        let sprite = new PIXI.Sprite(texture);
        sprite.pivot.x = texture.width / 2;
        sprite.pivot.y = texture.height / 2;
        sprite.scale.x = CARD_WIDTH / texture.width;
        sprite.scale.y = CARD_HEIGHT / texture.height;
        sprite.x = app.screen.width / 2;
        sprite.y = app.screen.height / 2;
        cardsContainer.addChild(sprite);
        return sprite;
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
        console.log('This state = ' + menuState + '. Next state');

        if (menuState === 0) {
            // Click on "Start the game"
            popupText.interactive = false;
            tweenTo(popupText, 'alpha', 0.0, 500, easeSquareRoot(), null, gameStart);
            menuState = 1;
        } else
        if (menuState === 1) {
            // Fade in "Game over"
            popupText.text = 'Game over';
            tweenTo(popupText, 'alpha', 1.0, 500, easeSquareRoot(), null, toggleState);
            menuState = 2;
        } else
        if (menuState === 2) {
            // "Game over"
            popupText.interactive = true;
            menuState = 3;
        } else
        if (menuState === 3) {
            // Click on "Game over"
            popupText.interactive = false;

            // Hide all existing cards
            for (const card of cards) {
                if (card.hasOwnProperty('sprite')) {
                    tweenTo(card.sprite, 'alpha', 0.0, 500, easeSquareRoot(), null, null);
                }
            }
            tweenTo(cardThrow, 'alpha', 0.0, 500, easeSquareRoot(), null, null);

            tweenTo(popupText, 'alpha', 0.0, 500, easeSquareRoot(), null, toggleState);

            menuState = 4;
        } else
        if (menuState === 4) {
            popupText.text = 'Start the game!';
            tweenTo(popupText, 'alpha', 1.0, 500, easeSquareRoot(), null, toggleState);

            menuState = 5;
        } else
        if (menuState === 5) {
            // Old cards hidden
            popupText.interactive = true;

            menuState = 0;
        }

        console.log('New state = ' + menuState);
    }

    function gameStart() {
        console.log('Game starts');

        currentCard = 0;
        cardThrowNew();
    }

    function cardThrowNew() {
        console.log('Throwing card ' + currentCard);
        // Make sure the thrown card is on top
        cardsContainer.removeChild(cardThrow);
        cardsContainer.addChild(cardThrow);

        cardThrow.alpha = 1.0;
        cardThrow.interactive = true;
        cardThrow.x = app.screen.width + CARD_WIDTH / 2;
        cardThrow.y = lerp(CARD_HEIGHT/2, app.screen.height - CARD_HEIGHT/2, Math.random());

        const tgtY = lerp(CARD_HEIGHT/2, app.screen.height - CARD_HEIGHT/2, Math.random());
        const tgtRotation = lerp(-10, 10, Math.random());
        const time = 3000 - currentCard * 50;

        // Remember tweens to cancel them on catch
        cardThrow.tweenX = tweenTo(cardThrow, 'x', -CARD_WIDTH / 2, time, easeLinear(), null, cardThrowComplete);
        cardThrow.tweenY = tweenTo(cardThrow, 'y', tgtY, time, easeLinear(), null,null);
        cardThrow.tweenR = tweenTo(cardThrow, 'rotation', tgtRotation, time, easeSin2(), null, null);
    }

    function cardThrowComplete() {
        console.log('Throw ended');

        if (currentCard < 2) {
            currentCard++;
            cardThrowNew();
        } else {
            console.log('Game ended');
            // Freeze the game (clear all pending animations)
            tweenStopAll();
            cardThrow.interactive = false;
            toggleState();
        }
    }

    function cardThrowCatch() {
        // Stop any card tweens
        tweenStop(cardThrow.tweenX);
        tweenStop(cardThrow.tweenY);
        tweenStop(cardThrow.tweenR);
        cardThrow.alpha = 0;
        cardThrow.interactive = false;

        // Flip the card
        if (!(cards[currentCard].hasOwnProperty('sprite'))) {
            cards[currentCard].sprite = createCardSprite(cards[currentCard].texture);
        }

        let cs = cards[currentCard].sprite;
        cs.alpha = 1.0;
        cs.x = cardThrow.x;
        cs.y = cardThrow.y;
        cs.rotation = cardThrow.rotation;

        //todo: Tween card to hand

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
function tweenStopAll() {
    tweening.splice(0, tweening.length);
}
// Listen for animate update
app.ticker.add((delta) => {
    const now = Date.now();
    for (const t of tweening) {
        const phase = Math.min(1, (now - t.start) / t.time);

        t.object[t.property] = lerp(t.propertyBeginValue, t.target, t.easing(phase));
        if (t.change) t.change(t);
        if (phase === 1) {
            t.object[t.property] = t.target;
            tweening.splice(tweening.indexOf(t), 1);
            // Call it last, cos it can reset all tweens
            if (t.complete) t.complete(t);
        }
    }
});

// Basic lerp function
function lerp(a1, a2, t) {
    return a1 * (1 - t) + a2 * t;
}

function easeLinear() {
    return (t) => (t);
}
function easeSquareRoot() {
    return (t) => (Math.sqrt(t));
}
function easeSin2() {
    return (t) => (Math.sin(Math.sin(t)));
}