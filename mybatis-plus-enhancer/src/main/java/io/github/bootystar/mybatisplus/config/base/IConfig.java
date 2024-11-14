package io.github.bootystar.mybatisplus.config.base;

import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;

import java.util.List;
import java.util.Map;

/**
 * 配置接口
 *
 * @author bootystar
 */
public interface IConfig {

    Map<String, Object> renderData(TableInfo tableInfo);

    List<CustomFile> getCustomFiles();

    boolean getFileOverride();
}
