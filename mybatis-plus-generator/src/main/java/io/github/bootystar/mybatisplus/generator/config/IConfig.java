package io.github.bootystar.mybatisplus.generator.config;

import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;

import java.util.List;
import java.util.Map;

/**
 * @Author booty
 * @Date 2023/9/15 16:54
 */
public interface IConfig {
    Map<String, Object>  renderData(TableInfo tableInfo);
    List<CustomFile> getCustomFiles();
    boolean getFileOverride();


}
