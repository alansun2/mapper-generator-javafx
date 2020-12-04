package com.alan344.service.generator;

import org.mybatis.generator.my.config.MybatisExportConfig;
import org.mybatis.generator.my.config.ServiceConfigThreadLocal;
import org.springframework.stereotype.Component;

/**
 * @author AlanSun
 * @date 2020/6/5 16:32
 */
@Component
public class MapperGeneratorStrategyContext {

    public MapperGeneratorStrategy getMapperGeneratorStrategy(MybatisExportConfig mybatisExportConfig) {
        final int selectTab = mybatisExportConfig.getSelectTab();
        if (ServiceConfigThreadLocal.getServiceConfig().isNotSkipService()) {
            switch (selectTab) {
                case 1:
                    return new MapperBaseWithServiceGenerator(mybatisExportConfig.getTkMybatisExportConfig());
                default:
                    return new MyMybatisWithServiceGeneratorService(mybatisExportConfig.getMybatisOfficialExportConfig());
            }
        } else {
            switch (selectTab) {
                case 1:
                    return new MapperBaseGenerator(mybatisExportConfig.getTkMybatisExportConfig());
                default:
                    return new MyMybatisGeneratorService(mybatisExportConfig.getMybatisOfficialExportConfig());
            }
        }
    }
}
