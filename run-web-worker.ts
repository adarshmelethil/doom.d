declare var lisp: any;

const worker = new Worker(new URL("web-worker.js", import.meta.url).href,
    {
        type: "module",
        deno: true,
    });

worker.onmessage = (output) => {
    const { message } = output.data;
    // This is safe because our callback is back
    // on the main thread.
    lisp.print(message);
}


worker.postMessage({ message: "You have my sword .... " });
