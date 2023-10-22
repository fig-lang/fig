import { initSync, wasm_main } from "./pkg/figc.js";

const WAT_MONACO_CONFIG = {
    keywords: [
        'get', 'set',
        'mut', 'func',
        'type', 'import',
        'memory', 'export',
        'data', 'global',
    ],
    symbols: /[=><!~?:&|+\-*\/\^%]+/,

    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    operators: [],

    typeKeywords: [
        'i32', 'i64', 'string', 'bool'
    ],

    // The main tokenizer for our languages
    tokenizer: {
        root: [
            // identifiers and keywords
            [/[a-z_$][\w$]*/, {
                cases: {
                    '@typeKeywords': 'keyword',
                    '@keywords': 'keyword',
                    '@default': 'identifier'
                }
            }],
            [/[A-Z][\w\$]*/, 'type.identifier'],  // to show class names nicely

            // whitespace
            { include: '@whitespace' },

            // delimiters and operators
            [/[{}()\[\]]/, '@brackets'],
            [/[<>](?!@symbols)/, '@brackets'],
            [/@symbols/, {
                cases: {
                    '@operators': 'operator',
                    '@default': ''
                }
            }],

            // @ annotations.
            // As an example, we emit a debugging log message on these tokens.
            // Note: message are supressed during the first load -- change some lines to see them.
            [/@\s*[a-zA-Z_\$][\w\$]*/, { token: 'annotation', log: 'annotation token: $0' }],

            // numbers
            [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
            [/0[xX][0-9a-fA-F]+/, 'number.hex'],
            [/\d+/, 'number'],

            // delimiter: after number because of .\d floats
            [/[;,.]/, 'delimiter'],

            // strings
            [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
            [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

            // characters
            [/'[^\\']'/, 'string'],
            [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
            [/'/, 'string.invalid']
        ],

        comment: [
            [/[^\;;]+/, 'comment'],
        ],

        string: [
            [/[^\\"]+/, 'string'],
            [/@escapes/, 'string.escape'],
            [/\\./, 'string.escape.invalid'],
            [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
        ],

        whitespace: [
            [/[ \t\r\n]+/, 'white'],
            [/\/\*/, 'comment', '@comment'],
            [/\/\/.*$/, 'comment'],
        ],
    }
}

const FIG_MONACO_CONFIG = {
    // Set defaultToken to invalid to see what you do not tokenize yet
    defaultToken: 'invalid',
    tokenPostfix: '.fig',

    keywords: [
        'fn', 'break', 'return', 'let', 'else', 'external', 'builtin', 'export', 'if', 'loop', 'const'],

    typeKeywords: [
        'i32', 'i64', 'string', 'bool'
    ],

    parenFollows: [
        'if', 'loop'
    ],

    operators: [
        '=', '>', '<', '!', '~', '?', ':', '==', '<=', '>=', '!=',
        '&&', '||', '++', '--', '+', '-', '*', '/', '&', '|', '^', '%',
        '<<', '>>', '>>>', '+=', '-=', '*=', '/=', '&=', '|=', '^=',
        '%=', '<<=', '>>=', '>>>='
    ],

    // we include these common regular expressions
    symbols: /[=><!~?:&|+\-*\/\^%]+/,

    // C# style strings
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    // The main tokenizer for our languages
    tokenizer: {
        root: [
            // identifiers and keywords
            [/[a-z_$][\w$]*/, {
                cases: {
                    '@typeKeywords': 'keyword',
                    '@keywords': 'keyword',
                    '@default': 'identifier'
                }
            }],
            [/[A-Z][\w\$]*/, 'type.identifier'],  // to show class names nicely

            // whitespace
            { include: '@whitespace' },

            // delimiters and operators
            [/[{}()\[\]]/, '@brackets'],
            [/[<>](?!@symbols)/, '@brackets'],
            [/@symbols/, {
                cases: {
                    '@operators': 'operator',
                    '@default': ''
                }
            }],

            // @ annotations.
            // As an example, we emit a debugging log message on these tokens.
            // Note: message are supressed during the first load -- change some lines to see them.
            [/@\s*[a-zA-Z_\$][\w\$]*/, { token: 'annotation', log: 'annotation token: $0' }],

            // numbers
            [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
            [/0[xX][0-9a-fA-F]+/, 'number.hex'],
            [/\d+/, 'number'],

            // delimiter: after number because of .\d floats
            [/[;,.]/, 'delimiter'],

            // strings
            [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
            [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

            // characters
            [/'[^\\']'/, 'string'],
            [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
            [/'/, 'string.invalid']
        ],

        comment: [
            [/[^\/*]+/, 'comment'],
            [/\/\*/, 'comment', '@push'],    // nested comment
            ["\\*/", 'comment', '@pop'],
            [/[\/*]/, 'comment']
        ],

        string: [
            [/[^\\"]+/, 'string'],
            [/@escapes/, 'string.escape'],
            [/\\./, 'string.escape.invalid'],
            [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
        ],

        whitespace: [
            [/[ \t\r\n]+/, 'white'],
            [/\/\*/, 'comment', '@comment'],
            [/\/\/.*$/, 'comment'],
        ],
    },
};

const STD_FUNCTIONS = `fn cmp_string(lhs: string, rhs: string): bool {
    let i: i32 = 0;

    loop {
        if (lhs[i] == 0) { break; }
        if (rhs[i] == 0) { break; }

        if (lhs[i] != rhs[i]) {
            return false;
        }

        i = i + 1;
    }

    return true;
}
`;

const PRELUDE = `external console { fn log(n: i32); fn log_str(s: string); }
builtin fn malloc(size: i32): i32;
builtin fn free(size: i32): i32;
`;

const MAIN_EXAMPLE = `export fn main() {
    let x: string = "Hello World";
    log_str(x);
}
`;

const fetch_source = async (path) =>
    await fetch(path);

const wasmInstance = (wasmModule) =>
    new WebAssembly.Module(wasmModule);

function read_chars(values, ptr) {
    let chars = [];
    let n = ptr;

    do {
        chars.push(values.getInt8(n));
        n += 1;
    } while (values.getInt8(n) !== 0);

    return chars;
}

function log_str(mem, ptr) {
    push_to_console(string_from_chars(read_chars(mem, ptr)));
}

class Reader {
    mem = null;

    constructor() {
        this.mem = null;
    }

    set_mem(exported_memory) {
        this.mem = exported_memory;
    }

    get_mem() {
        return this.mem;
    }
}

const DOM = {
    compile_btn: document.getElementById("compile-btn"),
    editor: document.getElementById("editor"),
    console: document.getElementById("console")
};

/**
 * @param {string} value 
 */
function push_to_console(value) {
    const item_element = document.createElement("p");
    item_element.innerHTML = value;

    DOM.console.append(item_element);
}

function string_from_chars(chars) {
    return chars.map(char => String.fromCharCode(char)).join("");
}

(async () => {
    const reader = new Reader();
    const exports = {
        fetch_export: null,
    };
    const imports = {
        console: {
            log_str: (ptr) => log_str(reader.get_mem(), ptr),
            log: (n) => push_to_console(n),
        },
        element: {
            // element ptr and text ptr
            create: (el, text) => {
                const element = string_from_chars(read_chars(reader.get_mem(), el));
                const child = document.createElement(element);
                const element_body = string_from_chars(read_chars(reader.get_mem(), text));
                child.innerText = element_body;
                window.document.body.appendChild(child);
            }
        },
        io: {
            fetch: async (url_ptr) => {
                const url = string_from_chars(read_chars(reader.get_mem(), url_ptr));
                const res = await fetch(url);
                const content = await res.text();

                const encoder = new TextEncoder();
                const encodedString = encoder.encode(content);

                const ptr = exports.malloc_wrapper(encodedString.length + 1);

                exports.fetch_export(ptr);
            }
        }
    };

    DOM.editor.value = MAIN_EXAMPLE;

    const wasm = await fetch_source("./pkg/figc_bg.wasm");
    const buf = await wasm.arrayBuffer();
    const mod = wasmInstance(buf);

    initSync(mod);

    const memory_offset = 0;

    require.config({ paths: { vs: './monaco-editor/min/vs' } });
    require(['vs/editor/editor.main'], async function() {
        monaco.languages.register({ id: "fig" });
        monaco.languages.setMonarchTokensProvider('fig', FIG_MONACO_CONFIG);

        monaco.languages.register({ id: "wat" });
        monaco.languages.setMonarchTokensProvider('wat', WAT_MONACO_CONFIG);

        let editor = monaco.editor.create(document.getElementById('editor'), {
            value: MAIN_EXAMPLE,
            language: 'fig',
            theme: "vs-dark",
            fontSize: "20px",
            automaticLayout: true,
            autoClosingBrackets: "always",
        });


        let wat_editor = monaco.editor.create(document.getElementById('wat-editor'), {
            value: ";; Compilation result in WebAssembly Text (Wat)",
            language: 'wat',
            theme: "vs-dark",
            automaticLayout: true,
            autoClosingBrackets: "always",
        });

        let wabt = new WabtModule();
        wabt = await wabt;

        DOM.compile_btn.addEventListener("click", async () => {
            DOM.console.innerHTML = "";
            try {
                const source = PRELUDE + STD_FUNCTIONS + editor.getValue();
                const result = wasm_main(source, memory_offset);
                const mod = wabt.readWasm(result, { readDebugNames: true });

                const wat = mod.toText({ foldExprs: false, inlineExport: false });

                wat_editor.setValue(wat);

                const program = wasmInstance(result);

                const wasm = await WebAssembly.instantiate(program, imports);
                //
                exports.fetch_export = wasm.exports.callback;
                console.log(wasm.exports);

                reader.set_mem(new DataView(wasm.exports.memory.buffer));

                wasm.exports.main();
            } catch (error) {
                push_to_console(error);
                console.log(error)
            }
        });
    });
})().catch(x => console.error(x))
