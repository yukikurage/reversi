import * as readline from "node:readline/promises";

export const questionImpl = (question: string) => () => {
  const readlineInterface = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  return readlineInterface.question(question);
};
