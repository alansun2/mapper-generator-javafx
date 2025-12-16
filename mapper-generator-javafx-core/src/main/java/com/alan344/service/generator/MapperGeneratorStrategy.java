package com.alan344.service.generator;

import com.alan344.bean.config.MybatisExportConfig;

/**
 * @author Alan
 * @since 2020/6/5 11:30
 * mapper 生成策略类
 */
public interface MapperGeneratorStrategy {
    /**
     * 生成 mapper
     *
     * @param mybatisExportConfig 配置
     */
    void generator(MybatisExportConfig mybatisExportConfig);
}
