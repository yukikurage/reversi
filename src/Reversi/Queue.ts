interface IQueue<T> {
  enqueue(item: T): void;
  dequeue(): T | undefined;
  size(): number;
}

class Queue<T> implements IQueue<T> {
  private storage: T[] = [];
  private head = 0;
  private tail = 0;

  enqueue(item: T): void {
    this.storage[this.tail++] = item;
  }

  dequeue(): T | undefined {
    if (this.head === this.tail) {
      return undefined;
    }
    const item = this.storage[this.head];
    delete this.storage[this.head++];
    return item;
  }

  size(): number {
    return this.tail - this.head;
  }
}

export const newQueue = <T>() => new Queue<T>();

export const enqueue =
  <T>(queue: IQueue<T>) =>
  (item: T) =>
  () =>
    queue.enqueue(item);

export const dequeueImpl =
  <T, U>(just: (val: T) => U) =>
  (nothing: U) =>
  (queue: IQueue<T>) =>
  () => {
    const item = queue.dequeue();
    return item === undefined ? nothing : just(item);
  };
