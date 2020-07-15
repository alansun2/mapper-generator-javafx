package com.alan344.service.generator;

import com.alan344.bean.GeneratorConfig;
import org.springframework.stereotype.Component;

/**
 * @author AlanSun
 * @date 2020/6/5 16:32
 */
@Component
public class MapperGeneratorStrategyContext {

    public MapperGeneratorStrategy getMapperGeneratorStrategy(GeneratorConfig generatorConfig) {
        final int selectTab = generatorConfig.getSelectTab();
        switch (selectTab) {
            case 2:
                return new MapperBaseGenerator(generatorConfig.getTkMybatisExportConfig());
            default:
                return new MyMybatisGeneratorService(generatorConfig.getMybatisExportConfig());
        }
    }
}
