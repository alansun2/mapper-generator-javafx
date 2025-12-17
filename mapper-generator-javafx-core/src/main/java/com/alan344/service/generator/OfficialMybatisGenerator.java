package com.alan344.service.generator;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.mybatisplugin.DomainPlugin;
import com.alan344.mybatisplugin.ExtraFileCustomTemplateGeneratorPlugin;
import com.alan344.mybatisplugin.ExtraFileJPAGeneratorPlugin;
import com.alan344.mybatisplugin.ExtraFileModelGeneratorPlugin;
import com.alan344.mybatisplugin.JpaAnnotationPlugin;
import com.alan344.mybatisplugin.MybatisGeneratorPlugin;
import com.alan344.mybatisplugin.SerializablePlugin;
import com.alan344.mybatisplugin.TinyIntToBooleanPlugin;
import com.alan344.mybatisplugin.ValidationAnnotationPlugin;
import com.github.uinio.mybatis.LombokPlugin;
import lombok.extern.slf4j.Slf4j;
import org.w3c.dom.Element;

import java.util.List;
import java.util.Optional;

/**
 * @author AlanSun
 * @since 2019/8/9 16:19
 */
@Slf4j
public class OfficialMybatisGenerator extends MapperGeneratorStrategyBase {
    public OfficialMybatisGenerator(MybatisExportConfig.ExportConfig exportConfig,
                                    List<MybatisPluginConfig> mybatisPluginConfigs) {
        super(exportConfig, mybatisPluginConfigs);
    }

    @Override
    public void addPlugin(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
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

        // tinyInt(1) 转 Boolean 插件
        if (exportConfig.isTinyInt1ToBoolean()) {
            generatorUtils.addPlugin(TinyIntToBooleanPlugin.class.getName());
        }

        // lombok 插件
        final Element lombok = generatorUtils.addPlugin(LombokPlugin.class.getName());
        generatorUtils.addProperty(exportConfig.isUseLombokGetSet(), lombok, "getter", "true");
        generatorUtils.addProperty(exportConfig.isUseLombokGetSet(), lombok, "setter", "true");
        generatorUtils.addProperty(exportConfig.isUseLombokBuilder(), lombok, "builder", "true");

        if (exportConfig.isUseValidationAnnotation()) {
            generatorUtils.addPlugin(ValidationAnnotationPlugin.class.getName());
        }

        if (exportConfig.isUseJpaAnnotation()) {
            generatorUtils.addPlugin(JpaAnnotationPlugin.class.getName());
        }

        if (mybatisExportConfig.isExportExtraFile()) {
            // 额外 model 生成插件
            generatorUtils.addPlugin(ExtraFileModelGeneratorPlugin.class.getName());
            // JPA
            generatorUtils.addPlugin(ExtraFileJPAGeneratorPlugin.class.getName());
            // 额外的模板文件生成插件
            generatorUtils.addPlugin(ExtraFileCustomTemplateGeneratorPlugin.class.getName());
        }

        // 用于控制是否生成对应的 mybatis 文件
        generatorUtils.addPlugin(MybatisGeneratorPlugin.class.getName());

        // 修改 domain 类名
        generatorUtils.addPlugin(DomainPlugin.class.getName());
    }
}
