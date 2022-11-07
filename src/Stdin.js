import * as readline from "node:readline/promises";

const readlineInterface = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
});

export const questionImpl = (question) => () => {
    return readlineInterface.question(question);
};
