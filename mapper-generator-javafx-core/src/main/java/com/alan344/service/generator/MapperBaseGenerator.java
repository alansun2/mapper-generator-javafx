package com.alan344.service.generator;

import com.alan344.bean.GeneratorConfig;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.my.plugin.TkMybatisKeySqlPlugin;
import org.w3c.dom.Element;

/**
 * @author AlanSun
 * @date 2020/6/5 12:37
 */
@Slf4j
public class MapperBaseGenerator extends MapperGeneratorStrategyBase {

    private final GeneratorConfig.TkMybatisExportConfig tkMybatisExportConfig;

    public MapperBaseGenerator(GeneratorConfig.ExportConfig exportConfig) {
        super(exportConfig);
        this.tkMybatisExportConfig = ((GeneratorConfig.TkMybatisExportConfig) exportConfig);
    }

    /**
     * 添加插件
     *
     * @param generatorUtils 工具
     */
    @Override
    protected void addPlugin(GeneratorUtils generatorUtils) {
        super.addPlugin(generatorUtils);
        final Element tkMybatisPlugin = generatorUtils.addPlugin(TkMybatisKeySqlPlugin.class.getName());
        generatorUtils.addProperty(tkMybatisExportConfig.isGenerateColumnConsts(), tkMybatisPlugin, "generateColumnConsts", tkMybatisExportConfig.isGenerateColumnConsts() + "");
    }
}
