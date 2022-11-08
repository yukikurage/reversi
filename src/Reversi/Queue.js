class Queue {
    constructor() {
        this.storage = [];
        this.head = 0;
        this.tail = 0;
    }
    enqueue(item) {
        this.storage[this.tail++] = item;
    }
    dequeue() {
        if (this.head === this.tail) {
            return undefined;
        }
        const item = this.storage[this.head];
        delete this.storage[this.head++];
        return item;
    }
    size() {
        return this.tail - this.head;
    }
}
export const newQueue = () => new Queue();
export const enqueue = (queue) => (item) => () => queue.enqueue(item);
export const dequeueImpl = (just) => (nothing) => (queue) => () => {
    const item = queue.dequeue();
    return item === undefined ? nothing : just(item);
};
