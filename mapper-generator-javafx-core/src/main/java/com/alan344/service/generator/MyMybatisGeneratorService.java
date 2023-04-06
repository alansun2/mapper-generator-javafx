package com.alan344.service.generator;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.bean.config.MybatisPluginConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * @author AlanSun
 * @date 2019/8/9 16:19
 */
@Slf4j
public class MyMybatisGeneratorService extends MapperGeneratorStrategyBase {
    public MyMybatisGeneratorService(MybatisExportConfig.ExportConfig exportConfig, List<MybatisPluginConfig> mybatisPluginConfigs) {
        super(exportConfig, mybatisPluginConfigs);
    }
}
