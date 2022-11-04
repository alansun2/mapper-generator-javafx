package com.alan344.service.generator;

import com.alan344.bean.config.MybatisExportConfig;
import org.springframework.stereotype.Component;

/**
 * @author AlanSun
 * @date 2020/6/5 16:32
 */
@Component
public class MapperGeneratorStrategyContext {

    public MapperGeneratorStrategy getMapperGeneratorStrategy(MybatisExportConfig mybatisExportConfig) {
        final int selectTab = mybatisExportConfig.getSelectTab();
        switch (selectTab) {
            default:
                return new MyMybatisGeneratorService(mybatisExportConfig.getMybatisOfficialExportConfig());
        }
    }
}
