package com.alan344.service.generator;

import org.mybatis.generator.my.config.MybatisExportConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * @author AlanSun
 * @date 2019/8/9 16:19
 */
@Slf4j
public class MyMybatisWithServiceGeneratorService extends MapperGeneratorStrategyBase {
    public MyMybatisWithServiceGeneratorService(MybatisExportConfig.ExportConfig exportConfig) {
        super(exportConfig);
    }
}
