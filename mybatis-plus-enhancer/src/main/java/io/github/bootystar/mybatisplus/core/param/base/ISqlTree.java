package io.github.bootystar.mybatisplus.core.param.base;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * SQL树
 * @author bootystar
 */
public interface ISqlTree extends Iterable<ISqlTree> {

    List<? extends ISqlCondition> getConditions();

    List<? extends ISqlSort> getSorts();

    ISqlTree getChild();

    @Override
    @SuppressWarnings("all")
    default Iterator<ISqlTree> iterator() {
        return new ISqlTree.Itr(this);
    }

    class Itr implements Iterator<ISqlTree> {

        private ISqlTree current;

        public Itr(ISqlTree root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public ISqlTree next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            current = current.getChild();
            return current;
        }
    }

}
