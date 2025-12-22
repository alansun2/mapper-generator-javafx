package com.alan344.bean.config;

import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.utils.NameUtils;
import com.alan344.utils.StringUtils;
import com.alibaba.fastjson2.JSON;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

/**
 * @author AlanSun
 * @since 2022/11/3 16:32
 */
@Getter
@Setter
public class ExtraTemplateFileConfig implements NameUtils.CheckNameRepeat, Cloneable {

    private String id;

    private String name;
    /**
     * 配置是否开启
     */
    private boolean enable;
    /**
     * 模板类型
     */
    private ExtraFileTypeEnum extraFileType = ExtraFileTypeEnum.MODEL;
    /**
     * 父类
     */
    private String superClass;

    /**
     * 默认的输出路径后缀
     */
    private String defaultOutputPathSuffix;

    /**
     * 默认的包名后缀
     */
    private String defaultPackageSuffix;

    /**
     * 当 TemplateTypeEnum 为 MODEL 时，可以指定后缀
     */
    private String modelSuffix;
    /**
     * 当 TemplateTypeEnum 为 MODEL 时，是否根据数据库生成 valid 注解
     */
    private boolean isGenerateValidAnnotation;
    /**
     * 当 TemplateTypeEnum 为 MODEL 时，是否生成 SpringDoc 注解
     */
    private boolean isGenerateSpringDocAnnotation;
    /**
     * 生成 model 时的忽略字段，逗号分隔
     */
    private String modelIgnoreColumns;

    private boolean lombokGetter;
    private boolean lombokSetter;
    private boolean lombokToString;

    /**
     * 自定义模板文件夹
     */
    private String customTemplateDir;

    /**
     * 文件输出地址
     */
    private String outputPath;
    /**
     * 包名
     */
    private String packageName;

    public void setModelIgnoreColumns(String modelIgnoreColumns) {
        if (StringUtils.isNotEmpty(modelIgnoreColumns)) {
            this.modelIgnoreColumns = modelIgnoreColumns.trim().replace(" ", "");
        }
    }

    @Override
    public ExtraTemplateFileConfig clone() {
        final ExtraTemplateFileConfig extraTemplateFileConfig = JSON.parseObject(JSON.toJSONString(this), ExtraTemplateFileConfig.class);
        extraTemplateFileConfig.setId(UUID.randomUUID().toString());
        return extraTemplateFileConfig;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ExtraTemplateFileConfig that = (ExtraTemplateFileConfig) o;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}