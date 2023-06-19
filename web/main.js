// For testing the Outputs

const fetch_source = async (path) =>
    await fetch(path);

const wasmInstance = async (wasmModule, imports) =>
    await WebAssembly.instantiateStreaming(wasmModule, imports);

const get_exports = (instance) => instance.instance.exports;

(async () => {
    const imports = {
        js: {
            log: (x) => console.log(x)
        },
        env: {
            sum: (x, y) => x + y
        }
    };

    let wasm = await fetch_source("./external.wasm");
    let exports = await wasmInstance(wasm, imports).then(ins => get_exports(ins));

    exports.main();
})()
    .catch(x => console.error(x))
