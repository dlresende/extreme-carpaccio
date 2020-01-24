import { sayHello } from "./calc";


describe("Test calc", () => {

    test("Example unit test", () => {
        expect(true).toBe(true);
    });

    it("say hello", () => {
        expect(sayHello()).toBe("Hello World");
    })

})