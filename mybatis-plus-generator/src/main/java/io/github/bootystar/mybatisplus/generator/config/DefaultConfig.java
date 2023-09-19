package io.github.bootystar.mybatisplus.generator.config;

import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;

import java.util.List;
import java.util.Map;

/**
 * @author booty
 * @since 2023/9/15 17:07
 */
public class DefaultConfig implements IConfig {
    @Override
    public Map<String, Object> renderData(TableInfo tableInfo) {
        return null;
    }

    @Override
    public List<CustomFile> getCustomFiles() {
        return null;
    }

    @Override
    public boolean getFileOverride() {
        return false;
    }
}
