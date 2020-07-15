package com.alan344.service.generator;

import com.alan344.bean.GeneratorConfig;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.my.plugin.TkMybatisKeySqlPlugin;

/**
 * @author AlanSun
 * @date 2020/6/5 12:37
 */
@Slf4j
public class MapperBaseGenerator extends MapperGeneratorStrategyBase {

    public MapperBaseGenerator(GeneratorConfig.ExportConfig exportConfig) {
        super(exportConfig);
    }

    /**
     * 添加插件
     *
     * @param generatorUtils
     */
    @Override
    protected void addPlugin(GeneratorUtils generatorUtils) {
        super.addPlugin(generatorUtils);
        generatorUtils.addPlugin(TkMybatisKeySqlPlugin.class.getName());
    }
}
