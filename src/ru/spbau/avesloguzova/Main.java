package ru.spbau.avesloguzova;

public class Main {

    public static void main(String[] args) {
	    Term I = new Lambda(new Variable("x"),new Variable("x"));
        System.out.println(I.reduce());
        System.out.println(new Application(I,I).reduce());
    }
}
