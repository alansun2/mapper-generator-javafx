package com.alan344.bean.config;

import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.utils.StringUtils;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

/**
 * @author AlanSun
 * @date 2022/11/3 16:32
 */
@Getter
@Setter
public class ExtraTemplateFileConfig implements Cloneable {

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
     * 文件输出地址
     */
    private String outputPath;
    /**
     * 父类
     */
    private String superClass;
    /**
     * 包名
     */
    private String packageName;
    /**
     * 当 TemplateTypeEnum 为 MODEL 时，可以指定后缀
     */
    private String modelSuffix;
    /**
     * 当 TemplateTypeEnum 为 MODEL 时，是否根据数据库生成 valid 注解
     */
    private boolean isGenerateValidAnnotation;
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

    public void setModelIgnoreColumns(String modelIgnoreColumns) {
        if (StringUtils.isNotEmpty(modelIgnoreColumns)) {
            this.modelIgnoreColumns = modelIgnoreColumns.trim().replace(" ", "");
        }
    }

    @Override
    public ExtraTemplateFileConfig clone() {
        try {
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            final ExtraTemplateFileConfig clone = (ExtraTemplateFileConfig) super.clone();
            clone.setId(UUID.randomUUID().toString());
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
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
