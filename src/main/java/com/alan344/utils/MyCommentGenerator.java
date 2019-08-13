package com.alan344.utils;

import org.mybatis.generator.api.CommentGenerator;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.MyBatisGenerator;
import org.mybatis.generator.api.dom.java.*;
import org.mybatis.generator.api.dom.xml.XmlElement;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.internal.util.StringUtility;

import java.text.SimpleDateFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Properties;
import java.util.Set;

import static org.mybatis.generator.internal.util.StringUtility.isTrue;

/**
 * @author Alan
 * @createtime 2017/9/27 * mybatis自定义注释
 */
public class MyCommentGenerator implements CommentGenerator {
    /**
     * The properties.
     */
    private Properties properties;

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
    private static final String COMMENT_GENERATOR_AUTHOR = "author"; //$NON-NLS-1$

    /**
     * Instantiates a new default comment generator.
     */
    public MyCommentGenerator() {
        super();
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
        String authorString = properties.getProperty(COMMENT_GENERATOR_AUTHOR);
        if (StringUtility.stringHasValue(authorString)) {
            this.author = authorString;
        }

        //swagger
        String supportSwaggerString = properties.getProperty("supportSwagger");
        if (StringUtility.stringHasValue(supportSwaggerString)) {
            this.supportSwagger = supportSwaggerString.equals("true");
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
        field.addJavaDocLine("* " + remarks);
        field.addJavaDocLine("*/");
        if (supportSwagger) {
            field.addJavaDocLine("@ApiModelProperty(value = \"" + remarks + "\")");
        }
    }

    @Override
    public void addFieldComment(Field field, IntrospectedTable introspectedTable) {

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
        StringBuilder sb = new StringBuilder();

        //添加导入类的信息
        if (supportSwagger) {
            topLevelClass.addJavaDocLine("import io.swagger.annotations.ApiModel;");
            topLevelClass.addJavaDocLine("import io.swagger.annotations.ApiModelProperty;");
        }
        topLevelClass.addJavaDocLine("import lombok.Getter;");
        topLevelClass.addJavaDocLine("import lombok.Setter;");

        //添加类注释
        topLevelClass.addJavaDocLine("/**");
        sb.append(" * ").append(remarks);
        sb.append("\n");
        sb.append(" * 实体类对应的数据表为：  ");
        sb.append(introspectedTable.getFullyQualifiedTable());
        topLevelClass.addJavaDocLine(sb.toString());
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
    public void addClassComment(InnerClass innerClass, IntrospectedTable introspectedTable) {

    }

    @Override
    public void addClassComment(InnerClass innerClass, IntrospectedTable introspectedTable, boolean b) {

    }

    @Override
    public void addEnumComment(InnerEnum innerEnum, IntrospectedTable introspectedTable) {

    }

    public void addGetterComment(Method method, IntrospectedTable introspectedTable, IntrospectedColumn introspectedColumn) {

    }

    public void addSetterComment(Method method, IntrospectedTable introspectedTable, IntrospectedColumn introspectedColumn) {

    }

    public void addGeneralMethodComment(Method method, IntrospectedTable introspectedTable) {

    }

    public void addJavaFileComment(CompilationUnit compilationUnit) {

    }

    public void addComment(XmlElement xmlElement) {

    }

    public void addRootComment(XmlElement xmlElement) {

    }

    @Override
    public void addGeneralMethodAnnotation(Method method, IntrospectedTable introspectedTable, Set<FullyQualifiedJavaType> imports) {

    }

    @Override
    public void addGeneralMethodAnnotation(Method method, IntrospectedTable introspectedTable, IntrospectedColumn introspectedColumn, Set<FullyQualifiedJavaType> imports) {

    }

    @Override
    public void addFieldAnnotation(Field field, IntrospectedTable introspectedTable, Set<FullyQualifiedJavaType> imports) {

    }

    @Override
    public void addFieldAnnotation(Field field, IntrospectedTable introspectedTable, IntrospectedColumn introspectedColumn, Set<FullyQualifiedJavaType> imports) {

    }

    private String getDateString() {
        if (this.suppressDate) {
            return null;
        } else {
            return this.dateFormat != null ? this.dateFormat.format(new Date()) : (new Date()).toString();
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
