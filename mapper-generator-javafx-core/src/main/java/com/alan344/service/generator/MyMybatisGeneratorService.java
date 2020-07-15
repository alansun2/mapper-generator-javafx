package com.alan344.service.generator;

import com.alan344.bean.GeneratorConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * @author AlanSun
 * @date 2019/8/9 16:19
 */
@Slf4j
public class MyMybatisGeneratorService extends MapperGeneratorStrategyBase {
    public MyMybatisGeneratorService(GeneratorConfig.ExportConfig exportConfig) {
        super(exportConfig);
    }
}