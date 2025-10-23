// A–Z Letter Learner
const LETTERS = 'abcdefghijklmnopqrstuvwxyz'.split('');

// Example words mapped for all letters. Each letter has 20 example words.
const EXAMPLES = {
    a: ['Apple', 'Ant', 'Airplane', 'Apricot', 'Anchor', 'Arrow', 'Alligator', 'Artist', 'Angel', 'Apron', 'Atlas', 'Atom', 'Alley', 'Alarm', 'Arcade', 'Avenue', 'Agenda', 'Almond', 'Album', 'Answer'],
    b: ['Ball', 'Banana', 'Butterfly', 'Bottle', 'Basket', 'Bridge', 'Button', 'Bed', 'Beach', 'Bell', 'Bench', 'Book', 'Bicycle', 'Biscuit', 'Boot', 'Brush', 'Branch', 'Bread', 'Builder', 'Bubble'],
    c: ['Cat', 'Car', 'Cup', 'Cake', 'Camera', 'Candle', 'Crayon', 'Cloud', 'Chair', 'Cheese', 'Clock', 'Chain', 'Castle', 'Coin', 'Corn', 'Card', 'Castle', 'Coconut', 'Circle', 'Cell'],
    d: ['Dog', 'Duck', 'Drum', 'Door', 'Desk', 'Dance', 'Doll', 'Diamond', 'Dish', 'Doctor', 'Dinosaur', 'Desk', 'Drive', 'Dream', 'Dust', 'Donut', 'Diver', 'Daisy', 'Donkey', 'Date'],
    e: ['Elephant', 'Egg', 'Engine', 'Eagle', 'Ear', 'Earth', 'Edge', 'Editor', 'Energy', 'Echo', 'Elm', 'Envelope', 'Era', 'Event', 'Evening', 'Estate', 'Exercise', 'Essay', 'Example', 'Exit'],
    f: ['Fish', 'Frog', 'Fan', 'Flag', 'Flower', 'Fork', 'Feather', 'Forest', 'Frame', 'Fruit', 'Film', 'Furnace', 'Fence', 'Festival', 'Finger', 'Floor', 'Flight', 'Fence', 'Fair', 'Festival'],
    g: ['Giraffe', 'Goat', 'Guitar', 'Gate', 'Garden', 'Glove', 'Glass', 'Grain', 'Game', 'Gold', 'Goal', 'Guest', 'Glen', 'Gum', 'Guide', 'Gadget', 'Garage', 'Gourd', 'Gaze', 'Galaxy'],
    h: ['House', 'Hat', 'Horse', 'Hammer', 'Hand', 'Helmet', 'Honey', 'Hill', 'Heart', 'Hook', 'Hotel', 'Hobby', 'Hut', 'Hose', 'Horizon', 'Herb', 'Hour', 'Hut', 'Hammock', 'Honeycomb'],
    i: ['Ice', 'Igloo', 'Insect', 'Island', 'Ink', 'Iron', 'Idea', 'Item', 'Image', 'Iris', 'Introduction', 'Instrument', 'Invoice', 'Icon', 'Indigo', 'Ivy', 'Impact', 'Issue', 'Island', 'Instructor'],
    j: ['Jam', 'Jelly', 'Jacket', 'Jar', 'Jog', 'Jewel', 'Jungle', 'Juice', 'Jury', 'Jet', 'Journal', 'Joke', 'Join', 'Judge', 'Jeans', 'Joystick', 'Journey', 'Junction', 'Jigsaw', 'Jazz'],
    k: ['Kite', 'Kangaroo', 'Key', 'Kitchen', 'Kettle', 'Kiss', 'Knot', 'Knight', 'Knowledge', 'Kernel', 'Kiosk', 'Kiwi', 'Kindle', 'Kale', 'King', 'Knit', 'Keel', 'Keeper', 'Kudos', 'Kayak'],
    l: ['Lion', 'Lamp', 'Leaf', 'Lake', 'Lemon', 'Ladder', 'Laptop', 'Letter', 'Library', 'Lace', 'Lobster', 'Lock', 'Lung', 'Loyal', 'Lesson', 'Leek', 'Light', 'Lily', 'Litter', 'Label'],
    m: ['Monkey', 'Moon', 'Mouse', 'Map', 'Milk', 'Mirror', 'Mango', 'Mountain', 'Machine', 'Magazine', 'Market', 'Mask', 'Method', 'Metal', 'Moment', 'Motor', 'Museum', 'Mouth', 'Movie', 'Model'],
    n: ['Nose', 'Nail', 'Net', 'Nest', 'Notebook', 'Night', 'Name', 'Neck', 'Needle', 'Number', 'Nature', 'Nectar', 'Nurse', 'Noise', 'Novel', 'Navigation', 'Nap', 'Nucleus', 'Navy', 'Neutral'],
    o: ['Orange', 'Octopus', 'Oven', 'Oasis', 'Ocean', 'Onion', 'Orbit', 'Office', 'Oil', 'Olive', 'Opera', 'Option', 'Organ', 'Origin', 'Oar', 'Outcome', 'Outlet', 'Overt', 'Oven', 'Owner'],
    p: ['Pig', 'Pencil', 'Piano', 'Paper', 'Paint', 'Planet', 'Peach', 'Pear', 'Pilot', 'Pump', 'Puzzle', 'Party', 'Palette', 'Phone', 'Picture', 'Price', 'Pocket', 'Parcel', 'Pillow', 'Pilot'],
    q: ['Queen', 'Quilt', 'Quail', 'Quarter', 'Query', 'Queue', 'Quick', 'Quiz', 'Quiver', 'Quarry', 'Quartz', 'Quote', 'Quality', 'Quench', 'Quantum', 'Quinoa', 'Quaint', 'Quorum', 'Question', 'Quokka'],
    r: ['Rabbit', 'Rainbow', 'Robot', 'River', 'Road', 'Ring', 'Rock', 'Rose', 'Rocket', 'Radio', 'Roof', 'Room', 'Rope', 'Recipe', 'Ruler', 'Report', 'Root', 'Race', 'Ranch', 'Recipe'],
    s: ['Sun', 'Snake', 'Star', 'Sand', 'Sock', 'Shoe', 'Ship', 'Stone', 'Sugar', 'School', 'Space', ' Spoon', 'Season', 'Sound', 'Stamp', 'Soup', 'Spoon', 'Scale', 'Stream', 'Signal'],
    t: ['Tree', 'Tiger', 'Truck', 'Table', 'Train', 'Turtle', 'Tent', 'Toast', 'Tower', 'Towel', 'Tool', 'Ticket', 'Theme', 'Token', 'Tomato', 'Tissue', 'Tea', 'Tide', 'Tablet', 'Task'],
    u: ['Umbrella', 'Unicorn', 'Uniform', 'Unit', 'Uncle', 'Underline', 'Universe', 'Usage', 'Utensil', 'Uplift', 'U-turn', 'Urgent', 'Utility', 'Urban', 'Ushers', 'Ultimate', 'Upgrade', 'Utopia', 'Use', 'Union'],
    v: ['Van', 'Violin', 'Vase', 'Valley', 'Value', 'Vast', 'Viper', 'Violet', 'Vine', 'Visitor', 'Vision', 'Vehicle', 'Vocal', 'Voice', 'Volunteer', 'Vote', 'Vault', 'Venue', 'Veteran', 'Vector'],
    w: ['Whale', 'Window', 'Wheel', 'Watch', 'Water', 'Wall', 'Wallet', 'Wagon', 'Wrist', 'Weight', 'Weather', 'Whisk', 'Web', 'Whisper', 'Wheat', 'Wave', 'Wonder', 'Wood', 'Workshop', 'Wolf'],
    x: ['Xylophone', 'X-ray', 'Box', 'Xenon', 'Xerox', 'Xylem', 'X-axis', 'Xenia', 'Xenial', 'Xeriscape', 'Xenophile', 'Xiphias', 'Xylograph', 'Xenagogue', 'Xerothermic', 'Xyst', 'Xenagogue', 'Xenolith', 'Xiphoid', 'Xenon'],
    y: ['Yak', 'Yacht', 'Yogurt', 'Yard', 'Year', 'Yarn', 'Yolk', 'Yell', 'Yellow', 'Yield', 'Youth', 'Yoga', 'Yew', 'Yonder', 'Yummy', 'Yodel', 'Yeast', 'Yap', 'Yardstick', 'Yule'],
    z: ['Zebra', 'Zoo', 'Zip', 'Zero', 'Zinc', 'Zone', 'Zucchini', 'Zipper', 'Zeal', 'Zigzag', 'Zenith', 'Zephyr', 'Zest', 'Zombie', 'Zonal', 'Zoom', 'Zodiac', 'Zither', 'Zebu', 'Zoning']
};

let currentIndex = 0;
let isUpper = true;

(function () {
    // robust root detection:
    // 1) try document.currentScript
    // 2) fallback: find the last script tag that references this script file
    // 3) finally, find the closest .alphabeta-embed container
    let scriptEl = document.currentScript;
    if (!scriptEl) {
        const scripts = Array.from(document.getElementsByTagName('script'));
        scriptEl = scripts.reverse().find(s => s.src && s.src.indexOf('/javascript/alphabeta/script.js') !== -1) || null;
    }
    const root = (scriptEl && scriptEl.closest && scriptEl.closest('.alphabeta-embed')) || document;

    const letterEl = root.querySelector ? root.querySelector('#letter') : null;
    const examplesList = root.querySelector ? root.querySelector('#examplesList') : null;
    const btnRandom = root.querySelector ? root.querySelector('#random') : null;
    const btnPrev = root.querySelector ? root.querySelector('#prev') : null;
    const btnNext = root.querySelector ? root.querySelector('#next') : null;
    const btnSpeak = root.querySelector ? root.querySelector('#speak') : null;
    const positionEl = root.querySelector ? root.querySelector('#position') : null;

    // guard: if running in a page where html wasn't inserted, exit silently
    if (!letterEl || !examplesList) return;

    function normalizeIndex(i) {
        return (i + LETTERS.length) % LETTERS.length;
    }

    function showLetter(index, opts = {}) {
        currentIndex = normalizeIndex(index);
        const ch = LETTERS[currentIndex];
        if (typeof opts.case === 'boolean') isUpper = opts.case;
        const display = isUpper ? ch.toUpperCase() : ch.toLowerCase();
        letterEl.textContent = display;
        letterEl.setAttribute('aria-label', `Letter ${display}`);
        if (positionEl) positionEl.textContent = `${currentIndex + 1} / ${LETTERS.length}`;
        renderExamples(ch);
    }

    function renderExamples(letter) {
        const pool = EXAMPLES[letter.toLowerCase()] || generateExamples(letter);
        const examples = sampleUnique(pool, 5);
        examplesList.innerHTML = '';
        examples.forEach(word => {
            const li = document.createElement('li');
            const btn = document.createElement('button');
            btn.type = 'button';
            btn.className = 'wordBtn';
            btn.innerHTML = highlightLetterInWord(word, letter);
            btn.setAttribute('aria-label', `Play ${word}`);
            btn.addEventListener('click', () => speakText(word));
            btn.addEventListener('keydown', (e) => {
                if (e.key === 'Enter' || e.code === 'Space') { e.preventDefault(); speakText(word); }
            });
            li.appendChild(btn);
            examplesList.appendChild(li);
        });
    }

    // Helper: return n unique random elements from array (if n >= arr.length, returns shuffled copy)
    function sampleUnique(arr, n) {
        const copy = arr.slice();
        // Fisher-Yates shuffle up to n
        for (let i = copy.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [copy[i], copy[j]] = [copy[j], copy[i]];
        }
        if (n >= copy.length) return copy;
        return copy.slice(0, n);
    }

    function highlightLetterInWord(word, letter) {
        const re = new RegExp(letter, 'ig');
        return word.replace(re, match => `<span class="highlight">${match}</span>`);
    }

    function generateExamples(letter) {
        // Fallback: create three simple pseudo-examples
        const cap = letter.toUpperCase();
        return [
            `${cap} word`,
            `${cap} example`,
            `${cap} something`
        ];
    }

    // Define control helpers BEFORE wiring event listeners
    function prev() { showLetter(currentIndex - 1); }
    function next() { showLetter(currentIndex + 1); }
    function random() { const idx = Math.floor(Math.random() * LETTERS.length); isUpper = Math.random() < 0.5; showLetter(idx); }

    function speakCurrent() {
        if (!window.speechSynthesis) return;
        const text = letterEl.textContent;
        const u = new SpeechSynthesisUtterance(text);
        u.rate = 0.9;
        window.speechSynthesis.cancel();
        window.speechSynthesis.speak(u);
    }

    function speakText(text) {
        if (!window.speechSynthesis) return;
        const u = new SpeechSynthesisUtterance(text);
        u.rate = 0.9;
        window.speechSynthesis.cancel();
        window.speechSynthesis.speak(u);
    }

    // wire controls (only if present)
    if (btnPrev) btnPrev.addEventListener('click', prev);
    if (btnNext) btnNext.addEventListener('click', next);
    if (btnRandom) btnRandom.addEventListener('click', random);
    if (letterEl) letterEl.addEventListener('click', () => { isUpper = !isUpper; showLetter(currentIndex); });
    if (btnSpeak) btnSpeak.addEventListener('click', () => speakCurrent());

    // keyboard handling scoped to the page (ignore when focus is in an input/textarea)
    window.addEventListener('keydown', (e) => {
        const active = document.activeElement;
        if (active && (active.tagName === 'INPUT' || active.tagName === 'TEXTAREA' || active.isContentEditable)) return;
        switch (e.key) {
            case 'ArrowLeft':
                prev(); e.preventDefault(); break;
            case 'ArrowRight':
                next(); e.preventDefault(); break;
            case ' ':
            case 'Spacebar':
                random(); e.preventDefault(); break;
            case 'Enter':
                speakCurrent(); e.preventDefault(); break;
            default:
                break;
        }
    });

    // initialize display
    showLetter(0, { case: true });
})();
