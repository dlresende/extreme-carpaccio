import { sayHello } from "./logic";


describe("Test server logic", () => {

    test("Example unit test", () => {
        expect(true).toBe(true);
    });

    it("say hello", () => {
        expect(sayHello()).toBe("Hello World");
    })

})