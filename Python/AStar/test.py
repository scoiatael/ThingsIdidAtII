class Test:
    CONST = 0
    
x = Test()
y = Test()
y.CONST = 5
print(Test.CONST)
x.CONST = 1
print(Test.CONST)
Test.CONST = 1
print(y.CONST)