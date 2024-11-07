package io.github.bootystar.mybatisplus.logic.splicing.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;
import java.util.function.Consumer;

/**
 * 递归条件
 *
 * @author bootystar
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConditionR implements Iterable<ConditionR> {

    /**
     * 条件
     */
    protected List<Condition> conditions;

    /**
     * 子条件
     * (满足父条件后的值才会筛选子条件)
     */
    protected ConditionR child;

    @Override
    @SuppressWarnings("all")
    public Iterator<ConditionR> iterator() {
        return new Itr();
    }

    @Override
    public void forEach(Consumer<? super ConditionR> action) {
        Itr iterator = new Itr();
        while (iterator.hasNext()) {
            action.accept(iterator.next());
        }
    }

    @Override
    public Spliterator<ConditionR> spliterator() {
        return Iterable.super.spliterator();
    }


    private class Itr implements Iterator<ConditionR> {

        private final Stack<ConditionR> stack = new Stack<>();

        public Itr() {
            stack.push(ConditionR.this);
        }

        @Override
        public boolean hasNext() {
            if (!stack.isEmpty()) {
                return true;
            }
            return false;
        }

        @Override
        public ConditionR next() {
            boolean empty = stack.isEmpty();
            if (empty) {
                throw new NoSuchElementException();
            }
            ConditionR pop = stack.pop();
            if (pop.getChild() != null) {
                stack.push(pop.getChild());
            }
            return pop;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("not support remove");
        }
    }
}
