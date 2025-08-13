package com.alan344.service.generator;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.service.MybatisPluginService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author AlanSun
 * @date 2020/6/5 16:32
 */
@Component
public class MapperGeneratorStrategyContext {
    @Autowired
    private MybatisPluginService mybatisPluginService;

    public MapperGeneratorStrategy getMapperGeneratorStrategy(MybatisExportConfig mybatisExportConfig) {
        final List<MybatisPluginConfig> byIds = mybatisPluginService.getByIds(mybatisExportConfig.getPluginIds());
        switch (mybatisExportConfig.getFrameworkType()) {
            case MYBATIS_FLEX:
                return new MybatisFlexGenerator(mybatisExportConfig.getMybatisExportConfig(), byIds);
            default:
                return new OfficialMybatisGenerator(mybatisExportConfig.getMybatisExportConfig(), byIds);
        }
    }
}
