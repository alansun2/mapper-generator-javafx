package com.alan344.comment;

import com.alan344happyframework.util.DateUtils;
import com.alan344happyframework.util.StringUtils;
import org.mybatis.generator.api.CommentGenerator;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.dom.java.*;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.internal.util.StringUtility;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.Set;

import static org.mybatis.generator.internal.util.StringUtility.isTrue;

/**
 * @author Alan
 * @date 2017/9/27 * mybatis自定义注释
 */
public class MyCommentGenerator implements CommentGenerator {
    /**
     * The properties.
     */
    private final Properties properties;

    /**
     * The suppress date.
     * 禁止日期生成·
     */
    private boolean suppressDate;

    /**
     * The suppress all comments.
     * 禁止生成所有的注释
     */
    private boolean suppressAllComments;

    /**
     * The addition of table remark's comments.
     * If suppressAllComments is true, this option is ignored
     * <p>
     * 添加表格字段的注释说明
     * 当suppressAllComments为true，这个参数的值将会被忽略
     */
    private boolean addRemarkComments;

    private SimpleDateFormat dateFormat;

    /**
     * 添加作者配置
     */
    private String author;

    /**
     * 是否生成swagger注释
     */
    private boolean supportSwagger;

    /**
     * 添加作者配置
     */
    private static final String COMMENT_GENERATOR_AUTHOR = "author";

    /**
     * Instantiates a new default comment generator.
     */
    public MyCommentGenerator() {
        properties = new Properties();
        suppressDate = false;
        suppressAllComments = false;
        addRemarkComments = false;
    }

    @Override
    public void addConfigurationProperties(Properties properties) {
        this.properties.putAll(properties);

        this.suppressDate = isTrue(properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_DATE));

        this.suppressAllComments = isTrue(properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS));

        this.addRemarkComments = isTrue(properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_ADD_REMARK_COMMENTS));

        String dateFormatString = properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_DATE_FORMAT);
        if (StringUtility.stringHasValue(dateFormatString)) {
            this.dateFormat = new SimpleDateFormat(dateFormatString);
        }

        //添加作者配置
        String authorString = this.properties.getProperty(COMMENT_GENERATOR_AUTHOR);
        if (StringUtility.stringHasValue(authorString)) {
            this.author = authorString;
        }

        //swagger
        String supportSwaggerString = this.properties.getProperty("supportSwagger");
        if (StringUtility.stringHasValue(supportSwaggerString)) {
            this.supportSwagger = "true".equals(supportSwaggerString);
        }
    }

    @Override
    public void addFieldComment(Field field, IntrospectedTable introspectedTable, IntrospectedColumn introspectedColumn) {
        if (suppressAllComments) {
            return;
        }

        //字段备注信息
        String remarks = introspectedColumn.getRemarks();
        field.addJavaDocLine("/**");
        field.addJavaDocLine(" * " + remarks);
        field.addJavaDocLine(" */");
        if (supportSwagger) {
            field.addJavaDocLine("@ApiModelProperty(value = \"" + remarks + "\")");
        }
    }

    @Override
    public void addModelClassComment(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        if (suppressAllComments || !addRemarkComments) {
            return;
        }
        //通过这种方式不能直接获取表备注
        String remarks = introspectedTable.getRemarks();
        //获取实体类名称
        String entityName = introspectedTable.getFullyQualifiedTable().getDomainObjectName();

        topLevelClass.addJavaDocLine("");
        //添加导入类的信息
        if (supportSwagger) {
            topLevelClass.addJavaDocLine("import io.swagger.annotations.ApiModel;");
            topLevelClass.addJavaDocLine("import io.swagger.annotations.ApiModelProperty;");
        }
        topLevelClass.addJavaDocLine("import lombok.Getter;");
        topLevelClass.addJavaDocLine("import lombok.Setter;");
        topLevelClass.addJavaDocLine("");

        //添加类注释
        topLevelClass.addJavaDocLine("/**");
        topLevelClass.addJavaDocLine(" * 实体类对应的数据表为：" + introspectedTable.getFullyQualifiedTable());
        topLevelClass.addJavaDocLine(" *");
        if (StringUtils.isNotEmpty(remarks)) {
            topLevelClass.addJavaDocLine(" * " + remarks);
            topLevelClass.addJavaDocLine(" *");
        }
        topLevelClass.addJavaDocLine(" * @author " + author);

        //添加时间
        topLevelClass.addJavaDocLine(" * @date " + getDateString());
        topLevelClass.addJavaDocLine(" */");
        topLevelClass.addJavaDocLine("");
        topLevelClass.addJavaDocLine("@Getter");
        topLevelClass.addJavaDocLine("@Setter");

        if (supportSwagger) {
            topLevelClass.addJavaDocLine("@ApiModel(value =\"" + entityName + "\")");
        }
    }

    @Override
    public void addMapperClassComment(Interface interfaze, IntrospectedTable introspectedTable) {
        // 作者信息
        interfaze.addJavaDocLine("/**");
        interfaze.addJavaDocLine(" * 对应的表：" + introspectedTable.getFullyQualifiedTable());
        interfaze.addJavaDocLine(" *");
        interfaze.addJavaDocLine(" * @author " + author);
        // 添加时间
        interfaze.addJavaDocLine(" * @date " + getDateString());
        interfaze.addJavaDocLine(" */");
    }

    private String getDateString() {
        if (this.suppressDate) {
            return null;
        } else {
            return this.dateFormat != null ? this.dateFormat.format(new Date()) : DateUtils.getCurrentDate();
        }
    }

    @Override
    public void addClassAnnotation(InnerClass innerClass, IntrospectedTable introspectedTable,
                                   Set<FullyQualifiedJavaType> imports) {
        for (FullyQualifiedJavaType anImport : imports) {
            innerClass.addJavaDocLine("import " + anImport.getFullyQualifiedName() + ";");
        }

        innerClass.addJavaDocLine("");
        for (FullyQualifiedJavaType anImport : imports) {
            innerClass.addJavaDocLine("@" + anImport.getShortName());
        }
    }
}
