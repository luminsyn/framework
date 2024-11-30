package io.github.bootystar.mybatisplus.enhance.helper;

public class Parent {
    private int x = 10; // 属性赋值

    private int y = 10;

    {
        System.out.println("父类实例代码块");
        System.out.println("x=" + x);
        System.out.println("y=" + y);
    }

    public Parent() {
        System.out.println("父类构造器");
        System.out.println("x=" + x);
        System.out.println("y=" + y);
    }


    public static void main(String[] args) {
        Child child = new Child();
    }
}



class Child extends Parent {
    private int x = 20; // 属性赋值
    private int y = 20; // 属性赋值

    {
        System.out.println("子类实例代码块");
        System.out.println("x=" + x);
        System.out.println("y=" + y);
    }

    public Child() {
        System.out.println("子类构造器");
        System.out.println("x=" + x);
        System.out.println("y=" + y);
    }
}