package com.alan344.service.generator;

import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.my.config.MybatisExportConfig;
import org.mybatis.generator.my.plugin.RequestGeneratorPlugin;
import org.mybatis.generator.my.plugin.SpringServiceGeneratorPlugin;
import org.mybatis.generator.my.plugin.TkMybatisKeySqlPlugin;
import org.w3c.dom.Element;

/**
 * @author AlanSun
 * @date 2020/6/5 12:37
 */
@Slf4j
public class MapperBaseWithServiceGenerator extends MapperGeneratorStrategyBase {

    private final MybatisExportConfig.TkMybatisExportConfig tkMybatisExportConfig;

    public MapperBaseWithServiceGenerator(MybatisExportConfig.ExportConfig exportConfig) {
        super(exportConfig);
        this.tkMybatisExportConfig = ((MybatisExportConfig.TkMybatisExportConfig) exportConfig);
    }

    /**
     * 添加插件
     *
     * @param generatorUtils 工具
     */
    @Override
    protected void addPlugin(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        super.addPlugin(generatorUtils, mybatisExportConfig);
        final Element tkMybatisPlugin = generatorUtils.addPlugin(TkMybatisKeySqlPlugin.class.getName());
        generatorUtils.addProperty(tkMybatisExportConfig.isGenerateColumnConsts(), tkMybatisPlugin, "generateColumnConsts", tkMybatisExportConfig.isGenerateColumnConsts() + "");

        generatorUtils.addPlugin(RequestGeneratorPlugin.class.getName());
        generatorUtils.addPlugin(SpringServiceGeneratorPlugin.class.getName());
    }
}
