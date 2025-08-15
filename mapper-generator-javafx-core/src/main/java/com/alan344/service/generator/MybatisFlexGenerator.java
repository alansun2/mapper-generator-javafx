package com.alan344.service.generator;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.mybatisplugin.DomainPlugin;
import com.alan344.mybatisplugin.ExtraFileCustomTemplateGeneratorPlugin;
import com.alan344.mybatisplugin.ExtraFileJPAlGeneratorPlugin;
import com.alan344.mybatisplugin.ExtraFileModelGeneratorPlugin;
import com.alan344.mybatisplugin.MybatisFlexPlugin;
import com.alan344.mybatisplugin.MybatisGeneratorPlugin;
import com.alan344.mybatisplugin.SerializablePlugin;
import com.alan344.mybatisplugin.ValidationAnnotationPlugin;
import com.github.uinio.mybatis.LombokPlugin;
import org.w3c.dom.Element;

import java.util.List;
import java.util.Optional;

/**
 * @author AlanSun
 * @since 2025/8/13 16:46
 */
public class MybatisFlexGenerator extends MapperGeneratorStrategyBase {
    public MybatisFlexGenerator(final MybatisExportConfig.ExportConfig exportConfig,
                                final List<MybatisPluginConfig> mybatisPluginConfigs) {
        super(exportConfig, mybatisPluginConfigs);
    }

    @Override
    protected void addPlugin(final GeneratorUtils generatorUtils, final MybatisExportConfig mybatisExportConfig) {
        // 添加自定义插件
        Optional.ofNullable(this.mybatisPluginConfigs).ifPresent(mybatisPluginConfigs -> {
            for (MybatisPluginConfig mybatisPluginConfig : mybatisPluginConfigs) {
                generatorUtils.addPlugin(mybatisPluginConfig.getClassName());
            }
        });
        // 添加序列化接口插件
        generatorUtils.addPlugin(SerializablePlugin.class.getName());
        final MybatisExportConfig.MybatisOfficialExportConfig exportConfig =
                mybatisExportConfig.getMybatisExportConfig();

        // lombok 插件
        final Element lombok = generatorUtils.addPlugin(LombokPlugin.class.getName());
        generatorUtils.addProperty(exportConfig.isUseLombokGetSet(), lombok, "getter", "true");
        generatorUtils.addProperty(exportConfig.isUseLombokGetSet(), lombok, "setter", "true");
        generatorUtils.addProperty(exportConfig.isUseLombokBuilder(), lombok, "builder", "true");

        generatorUtils.addPlugin(MybatisFlexPlugin.class.getName());

        if (exportConfig.isUseValidationAnnotation()) {
            generatorUtils.addPlugin(ValidationAnnotationPlugin.class.getName());
        }

        if (mybatisExportConfig.isExportExtraFile()) {
            // 额外 model 生成插件
            generatorUtils.addPlugin(ExtraFileModelGeneratorPlugin.class.getName());
            // JPA
            generatorUtils.addPlugin(ExtraFileJPAlGeneratorPlugin.class.getName());
            // 额外的模板文件生成插件
            generatorUtils.addPlugin(ExtraFileCustomTemplateGeneratorPlugin.class.getName());
        }

        // 用于控制是否生成对应的 mybatis 文件
        generatorUtils.addPlugin(MybatisGeneratorPlugin.class.getName());

        // 修改 domain 类名
        generatorUtils.addPlugin(DomainPlugin.class.getName());
    }
}
