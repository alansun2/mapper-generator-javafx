package org.mybatis.generator.my.comment;

import com.alan344happyframework.util.DateUtils;
import com.alan344happyframework.util.StringUtils;
import org.mybatis.generator.api.CommentGenerator;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.dom.java.*;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.internal.util.StringUtility;

import java.util.Properties;
import java.util.Set;

/**
 * @author Alan
 * @date 2017/9/27 * mybatis自定义注释
 */
public class MyCommentGenerator implements CommentGenerator {
    /**
     * The properties.
     */
    private final Properties properties = new Properties();

    /**
     * 添加表格字段的注释说明
     */
    private boolean addRemarkComments;

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

    @Override
    public void addConfigurationProperties(Properties properties) {
        this.properties.putAll(properties);

        this.addRemarkComments = StringUtility.isTrue(this.properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_ADD_REMARK_COMMENTS));

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
        String remarks = introspectedColumn.getRemarks();

        if (addRemarkComments) {
            final String remark = remarks.replaceAll("\n", "\n\t * ");
            //字段备注信息
            field.addJavaDocLine("/**");
            field.addJavaDocLine(" * " + remark);
            field.addJavaDocLine(" */");
        }

        if (supportSwagger) {
            final String remark = remarks.replaceAll("\n", ",");
            field.addJavaDocLine("@ApiModelProperty(value = \"" + remark + "\")");
        }
    }

    @Override
    public void addModelClassComment(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
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

        if (addRemarkComments) {
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
            topLevelClass.addJavaDocLine(" * @date " + DateUtils.getCurrentDate());
            topLevelClass.addJavaDocLine(" */");
        }

        topLevelClass.addJavaDocLine("");
        topLevelClass.addJavaDocLine("@Getter");
        topLevelClass.addJavaDocLine("@Setter");

        if (supportSwagger) {
            topLevelClass.addJavaDocLine("@ApiModel(value =\"" + entityName + "\")");
        }
    }

    @Override
    public void addMapperClassComment(Interface interfaze, IntrospectedTable introspectedTable) {
        if (!addRemarkComments) {
            return;
        }
        // 作者信息
        interfaze.addJavaDocLine("/**");
        interfaze.addJavaDocLine(" * 对应的表：" + introspectedTable.getFullyQualifiedTable());
        interfaze.addJavaDocLine(" *");
        interfaze.addJavaDocLine(" * @author " + author);
        // 添加时间
        interfaze.addJavaDocLine(" * @date " + DateUtils.getCurrentDate());
        interfaze.addJavaDocLine(" */");
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
