"use strict";

// Global resource, meh
const URL_IMG = 'https://deckofcardsapi.com/static/img/';
const URL_NEW_DECK = 'https://deckofcardsapi.com/api/deck/new/draw/?count=52';

let menuState = 0;
let cards = [];
let cardsLookup = [];
let cardsCaught = [];
const cardBack = {name: 'back', url: URL_IMG + 'back.png'};

const CARDS_COUNT = 52;
const CARD_WIDTH = 226 * 0.5;
const CARD_HEIGHT = 314 * 0.5;
const THROW_LEN = 3000;
const THROW_DEC = 50;
const CARDS_IN_FAN = 15;
const FAN_BLOAT = 8;
const FAN_ARCHING = 50;
const FAN_ANGLE_MIN = 10;
const FAN_ANGLE_MAX = 30;

// Do once on startup, so we could generate our textures
// They are not gonna change, right?
generateListOfCards();
function generateListOfCards() {
    const ranks = ['2', '3', '4', '5', '6', '7', '8', '9', '0', 'J', 'Q', 'K', 'A'];
    const suits = ['C', 'D', 'H', 'S'];

    for (const r of ranks) {
        for (const s of suits) {
            let backfaceUrl = URL_IMG + r + s + '.png';
            if (r + s === 'AD') {
                backfaceUrl = URL_IMG + 'aceDiamonds.png';
            }

            cards.push({
                name: r + s,
                url: backfaceUrl
            });
        }
    }
}

const app = new PIXI.Application({ backgroundColor: 0x206020 });
document.body.appendChild(app.view);

app.loader
    .add(cardBack) // load name/url
    .add(cards) // load names/urls
    .load(onAssetsLoaded);


// onAssetsLoaded handler builds the example.
function onAssetsLoaded() {

    const cardsContainer = createTable();
    function createTable() {
        let cc = new PIXI.Container();
        cc.y = 0;
        cc.x = 0;
        app.stage.addChild(cc);
        return cc;
    }

    loadTextures();
    function loadTextures() {
        cardBack.texture = PIXI.Texture.from(cardBack.name);
        for (const card of cards) {
            card.texture = PIXI.Texture.from(card.name);
            card.sprite = createCardSprite(card.texture);
        }
    }

    const cardThrow = createCardSprite(cardBack.texture);
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
        sprite.alpha = 0;
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
            dropShadowDistance: 6
        });

        let newText = new PIXI.Text('Loading ...', style);
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

    // Start preparing right away
    toggleState();
    function toggleState() {
        console.log('This state = ' + menuState + '. Next state');

        if (menuState === 0) {
            // Load new deck before we can start new round
            gamePrepare(toggleState);
            menuState = 1;
        } else
        if (menuState === 1) {
            popupText.text = 'Start the game!';
            popupText.interactive = true;
            popupText.x = Math.round((app.screen.width - popupText.width) / 2);
            menuState = 2;
        } else
        if (menuState === 2) {
            // Click on "Start the game"
            popupText.interactive = false;
            tweenTo(popupText, 'alpha', 0.0, 500, easeSquareRoot(), null, gameStart);
            menuState = 3;
        } else
        if (menuState === 3) {
            // Fade in "Game over"
            popupText.text = 'Game over';
            popupText.x = Math.round((app.screen.width - popupText.width) / 2);
            tweenTo(popupText, 'alpha', 1.0, 500, easeSquareRoot(), null, toggleState);
            menuState = 4;
        } else
        if (menuState === 4) {
            // "Game over"
            popupText.interactive = true;
            menuState = 5;
        } else
        if (menuState === 5) {
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

            menuState = 6;
        } else
        if (menuState === 6) {
            popupText.text = 'Loading ...';
            popupText.x = Math.round((app.screen.width - popupText.width) / 2);
            tweenTo(popupText, 'alpha', 1.0, 500, easeSquareRoot(), null, toggleState);

            menuState = 0;
        }

        console.log('New state = ' + menuState);
    }

    function gamePrepare(callback) {
        cardsLookup = [];
        cardsCaught = [];

        fetch(URL_NEW_DECK)
            .then(res => res.json())
            .then(function (data) {
                // We can cache the entire deck upfront
                // Note: this optimization allows the player to lookup the deck in dev console
                for (const dc of data.cards) {
                    let found = false;
                    for (const c of cards)
                        if (c.name === dc.code) {
                            found = true;
                            cardsLookup.push(c);
                            break;
                        }
                    if (!found) console.log('Card not found ' + dc.code);
                }
                callback();
            })
            .catch(console.log('Catch'));
    }

    function gameStart() {
        currentCard = 0;
        cardThrowNew();
    }

    function cardThrowNew() {
        console.log('Throwing card #' + (currentCard+1));
        // Make sure the thrown card is on top
        cardsContainer.removeChild(cardThrow);
        cardsContainer.addChild(cardThrow);

        cardThrow.alpha = 1.0;
        cardThrow.interactive = true;
        cardThrow.x = app.screen.width + CARD_WIDTH / 2;
        cardThrow.y = lerp(CARD_HEIGHT/2, app.screen.height - CARD_HEIGHT/2, Math.random());

        const tgtY = lerp(CARD_HEIGHT/2, app.screen.height - CARD_HEIGHT/2, Math.random());
        const tgtRotation = lerp(-10, 10, Math.random());
        const time = THROW_LEN - currentCard * THROW_DEC;

        // Remember tweens to cancel them on catch
        cardThrow.tweenX = tweenTo(cardThrow, 'x', -CARD_WIDTH / 2, time, easeLinear(), null, cardThrowComplete);
        cardThrow.tweenY = tweenTo(cardThrow, 'y', tgtY, time, easeLinear(), null,null);
        cardThrow.tweenR = tweenTo(cardThrow, 'rotation', tgtRotation, time, easeSin2(), null, null);
    }

    function cardThrowComplete() {
        if (currentCard < CARDS_COUNT-1) {
            currentCard++;
            cardThrowNew();
        } else {
            console.log('Game ended');
            console.log('Caught ' + cardsCaught.length + ' cards');
            cardThrow.interactive = false;
            toggleState();
        }
    }

    function cardThrowCatch() {
        let card = cardsLookup[currentCard];
        console.log('Caught card #' + currentCard + '. It is a ' + card.name);

        cardsCaught.push(card);

        // Stop any current card tweens
        tweenStop(cardThrow.tweenX);
        tweenStop(cardThrow.tweenY);
        tweenStop(cardThrow.tweenR);
        cardThrow.alpha = 0;
        cardThrow.interactive = false;

        // Flip the card
        {
            let cs = card.sprite;

            // Make sure the caught card is on top
            cardsContainer.removeChild(cs);
            cardsContainer.addChild(cs);
            cs.alpha = 1.0;
            cs.x = cardThrow.x;
            cs.y = cardThrow.y;
            cs.rotation = 0;
        }

        // Tween card to hand
        handCardsRearrange();

        // Throw next card
        cardThrowComplete();
    }

    function handCardsRearrange() {
        // Calculate position for each visible card

        let topFan = Math.floor((cardsCaught.length - 1) / CARDS_IN_FAN);
        let cntMin = (cardsCaught.length - 1) % CARDS_IN_FAN + 1;

        for (let i = 0; i < cardsCaught.length; i++) {
            const cs = cardsCaught[i].sprite;
            let fan = Math.floor(i / CARDS_IN_FAN);
            let idx = i % CARDS_IN_FAN;

            let cardsInFan = (fan === topFan) ? cntMin : CARDS_IN_FAN;
            let cardCoef = idx - (cardsInFan - 1) / 2;

            let cardsAngle = Math.min(CARDS_IN_FAN * FAN_ANGLE_MIN / cardsInFan, FAN_ANGLE_MAX);
            let fanSpacing = (app.screen.width / 1.5) / (topFan + 1);
            let fanPlacement = app.screen.width / 2 + (fan - topFan / 2) * fanSpacing;

            // Place cards in an arch (nicer looking fan)
            let arch = cardsInFan > 2 ? Math.sin((1 - Math.abs(idx / (cardsInFan-1) * 2 - 1))) : 0;

            let x = fanPlacement + cardCoef * FAN_BLOAT;
            let y = app.screen.height * 0.99 - CARD_HEIGHT / 2 - (arch / CARDS_IN_FAN * (idx+1)) * FAN_ARCHING;
            let r = cardCoef * cardsAngle;
            r = r / 180 * Math.PI;

            tweenTo(cs, 'x', x, 500, easeSquareRoot(), null, null);
            tweenTo(cs, 'y', y, 500, easeSquareRoot(), null, null);
            tweenTo(cs, 'rotation', r, 500, easeSquareRoot(), null, null);
        }
    }
}

// Very simple tweening utility function. This should be replaced with a proper tweening library in a real product
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
    return a1 + (a2 - a1) * t;
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