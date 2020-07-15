package com.alan344.service.generator;

import com.alan344.bean.GeneratorConfig;

/**
 * @author Alan
 * @date 2020/6/5 11:30
 * mapper 生成策略类
 */
public interface MapperGeneratorStrategy {
    /**
     * 生成 mapper
     *
     * @param generatorConfig 配置
     */
    void generator(GeneratorConfig generatorConfig);
}
