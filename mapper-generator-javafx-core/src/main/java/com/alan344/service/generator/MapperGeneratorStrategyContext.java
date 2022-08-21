package com.alan344.service.generator;

import com.alan344.bean.MybatisExportConfig;
import com.alan344.bean.ServiceConfigThreadLocal;
import org.springframework.stereotype.Component;

/**
 * @author AlanSun
 * @date 2020/6/5 16:32
 */
@Component
public class MapperGeneratorStrategyContext {

    public MapperGeneratorStrategy getMapperGeneratorStrategy(MybatisExportConfig mybatisExportConfig) {
        final int selectTab = mybatisExportConfig.getSelectTab();
        if (ServiceConfigThreadLocal.getServiceConfig() != null) {
            switch (selectTab) {
                default:
                    return new MyMybatisWithServiceGeneratorService(mybatisExportConfig.getMybatisOfficialExportConfig());
            }
        } else {
            switch (selectTab) {
                default:
                    return new MyMybatisGeneratorService(mybatisExportConfig.getMybatisOfficialExportConfig());
            }
        }
    }
}
