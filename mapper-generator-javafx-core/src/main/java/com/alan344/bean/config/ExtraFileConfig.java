package com.alan344.bean.config;

import com.alan344.constants.ExtraFileTypeEnum;
import com.alan344.utils.StringUtils;
import lombok.Getter;
import lombok.Setter;

/**
 * @author AlanSun
 * @date 2022/11/3 16:32
 */
@Getter
@Setter
public class ExtraFileConfig implements Cloneable {

    private String name;
    /**
     * 配置是否开启
     */
    private boolean enable;
    /**
     * 模板类型
     */
    private ExtraFileTypeEnum templateType;
    /**
     * 文件输出地址
     */
    private String outputPath;
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
    /**
     * 自定义默认的地址
     */
    private String customTemplateInputPath;

    public void setModelIgnoreColumns(String modelIgnoreColumns) {
        if (StringUtils.isNotEmpty(modelIgnoreColumns)) {
            this.modelIgnoreColumns = modelIgnoreColumns.trim().replaceAll(" ", "");
        }
    }

    @Override
    public ExtraFileConfig clone() {
        try {
            ExtraFileConfig clone = (ExtraFileConfig) super.clone();
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }
}
