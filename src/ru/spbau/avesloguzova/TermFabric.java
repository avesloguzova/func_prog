package ru.spbau.avesloguzova;

/**
 * Created by av on 2/19/15.
 */
public class TermFabric {
    public static Variable getVariable(String name) {
        return new Variable(name);
    }

    public static Lambda getLambda(Variable var, Term term) {
        return new Lambda(var, term);
    }

    public static Application getApplication(Term first, Term secound) {
        return new Application(first, secound);
    }
}
