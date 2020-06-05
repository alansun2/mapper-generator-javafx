package com.alan344.service.generator;

import com.alan344.bean.GeneratorConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author AlanSun
 * @date 2020/6/5 16:32
 */
@Component
public class MapperGeneratorStrategyContext {
    @Autowired
    private MapperBaseGenerator mapperBaseGenerator;

    @Autowired
    private MyMybatisGeneratorService myMybatisGeneratorService;

    public MapperGeneratorStrategy getMapperGeneratorStrategy(GeneratorConfig generatorConfig) {
        final boolean useTkMybatis = generatorConfig.isUseTkMybatis();
        if (useTkMybatis) {
            return mapperBaseGenerator;
        } else {
            return myMybatisGeneratorService;
        }
    }
}
