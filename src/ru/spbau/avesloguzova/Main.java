package ru.spbau.avesloguzova;

import static ru.spbau.avesloguzova.TermFabric.*;
public class Main {

    public static void main(String[] args) {
        /*
        Variable x = getVariable("x");
        Variable y = getVariable("y");
        Variable z = getVariable("z");
        Term test = getApplication(getApplication(getLambda(x, getLambda(y, getLambda(z, getApplication(x, y)))), y), z);
        */
        Term zero = Lam("f", Lam("y", Var("y")));
        Term one = Lam("f", Lam("y", Apps(Var("f"), Var("y"))));
        Term two = Lam("f", Lam("y", Apps(Var("f"), Apps(Var("f"), Var("y")))));
        Term three = Lam("f", Lam("y", Apps(Var("f"), Apps(Var("f"), Apps(Var("f"), Var("y"))))));
        Term suc = Lam("n", Lam("f", Lam("y", Apps(Var("n"), Var("f"), Apps(Var("f"), Var("y"))))));
        Term plus = Lam("n", Lam("m", Apps(Var("n"), suc, Var("m"))));

        Term mul = Lam("n", Lam("m", Apps(Var("n"), (Apps(plus, Var("m"))), zero)));
        Term test1 = Apps(mul, one, two);

//        Term two_ = Apps(plus, one, one);
//        Term test2 = Apps(plus, two_, one);
//        System.out.println(two_.reduce());
        System.out.println(test1.reduce());
//        System.out.println(test2.reduce());


//	    Term I = getLambda(x, x);
//        Term K = getLambda(x, getLambda(y, x));
//        Term S = getLambda(x,getLambda(y,getLambda(z, getApplication(getApplication(x, z), getApplication(y, z)))));
//        System.out.println(I.reduce());
//        System.out.println(getApplication(I, I).reduce());
//
//        System.out.println(getApplication(S,K).reduce());
//        System.out.println(getApplication(getApplication(S,K),S).reduce());
    }
}
