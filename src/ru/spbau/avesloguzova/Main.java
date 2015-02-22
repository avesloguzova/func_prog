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
        Term two = Lam("x", Lam("y", Apps(Var("x"), Apps(Var("x"), Var("y")))));
        Term three = Lam("x", Lam("y", Apps(Var("x"), Apps(Var("x"), Apps(Var("x"), Var("y"))))));
        Term suc = Lam("n", Lam("x", Lam("y", Apps(Var("n"), Var("x"), Apps(Var("x"), Var("y"))))));
        Term plus = Lam("n", Lam("m", Apps(Var("n"), suc, Var("m"))));
        Term test = Apps(plus, three, three);
        System.out.println(test);
        System.out.println(test.reduce());
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
