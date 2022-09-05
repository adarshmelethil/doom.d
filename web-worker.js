
self.onmessage = (input) => {
    let { message } = input.data;
    message += " And My Axe ";
    self.postMessage({ message });
    self.close();
};
