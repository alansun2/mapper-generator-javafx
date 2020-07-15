package com.alan344.service.generator;

import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.my.plugin.TkMybatisKeySqlPlugin;
import org.springframework.stereotype.Service;

/**
 * @author AlanSun
 * @date 2020/6/5 12:37
 */
@Slf4j
@Service
public class MapperBaseGenerator extends MapperGeneratorStrategyBase {

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
