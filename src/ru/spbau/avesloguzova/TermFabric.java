package ru.spbau.avesloguzova;

/**
 * Created by av on 2/19/15.
 */
public class TermFabric {
    public static Variable Var(String name) {
        return new Variable(name);
    }

    public static Lambda Lam(String var, Term term) {
        return new Lambda(Var(var), term);
    }

    public static Term Apps(Term first, Term... rest) {
        for (Term term : rest) {
            first = new Application(first, term);
        }
        return first;
    }
}
