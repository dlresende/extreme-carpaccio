
describe('Configuration', function(){
   it('should evaluate function from string', function() {
       var customEval = function(s) { return new Function("return " + s)(); },
           str1 = "function (price) { return price - 50; }",
           fun1 = customEval(str1),
           str2 = "function (price) { return price / 2; }",
           fun2 = customEval(str2);

       expect(fun1.apply(null, [124])).toEqual(74);
       expect(fun2.apply(null, [124])).toEqual(62);
   })
});