package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.config.SimpleConfig;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * 代码堆叠生成器
 *
 * @author bootystar
 */
public class SimpleGenerator extends AbstractGenerator<SimpleConfig.Builder> {

    public SimpleGenerator(String url, String username, String password) {
        super(url, username, password,new SimpleConfig.Builder());
    }


    @Override
    protected void config4child() {

    }

    @Override
    protected void config4oldTemplate() {

    }

//    @Override
//    protected void config4newTemplate() {
//
//    }
}
