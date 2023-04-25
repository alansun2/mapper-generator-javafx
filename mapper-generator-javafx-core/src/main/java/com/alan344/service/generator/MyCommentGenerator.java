package com.alan344.service.generator;

import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.JavaElement;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.config.MergeConstants;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.internal.DefaultCommentGenerator;
import org.mybatis.generator.internal.util.StringUtility;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Properties;

import static org.mybatis.generator.internal.util.StringUtility.isTrue;

/**
 * @author AlanSun
 * @date 2023/4/26 1:43
 * <p>
 * mybatis-generator 自定义注释生成器
 **/
public class MyCommentGenerator extends DefaultCommentGenerator {

    private final Properties properties = new Properties();

    private boolean suppressDate;

    private boolean suppressAllComments;

    /**
     * If suppressAllComments is true, this option is ignored.
     */
    private boolean addRemarkComments;

    private DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("yyyy-dd-MM HH:mm:ss");

    private String author;

    public MyCommentGenerator() {
        super();
        suppressDate = false;
        suppressAllComments = false;
        addRemarkComments = false;
    }

    @Override
    public void addConfigurationProperties(Properties props) {
        this.properties.putAll(props);

        suppressDate = isTrue(properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_DATE));

        suppressAllComments = isTrue(properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS));

        addRemarkComments = isTrue(properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_ADD_REMARK_COMMENTS));

        author = properties.getProperty("author");

        String dateFormatString = properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_DATE_FORMAT);
        if (StringUtility.stringHasValue(dateFormatString)) {
            dateFormat = DateTimeFormatter.ofPattern(dateFormatString);
        }
    }

    /**
     * This method adds the custom javadoc tag for. You may do nothing if you do not
     * wish to include the Javadoc tag - however, if you do not include the Javadoc
     * tag then the Java merge capability of the eclipse plugin will break.
     *
     * @param javaElement       the java element
     * @param markAsDoNotDelete the mark as do not delete
     */
    protected void addJavadocTag(JavaElement javaElement, boolean markAsDoNotDelete) {
        javaElement.addJavaDocLine(" *");
        StringBuilder sb = new StringBuilder();
        sb.append(" * ");
        sb.append(MergeConstants.NEW_ELEMENT_TAG);
        if (markAsDoNotDelete) {
            sb.append(" do_not_delete_during_merge");
        }
        String s = getDateString();
        if (s != null) {
            sb.append(' ');
            sb.append(s);
        }
        javaElement.addJavaDocLine(sb.toString());
    }

    /**
     * Returns a formated date string to include in the Javadoc tag and XML
     * comments. You may return null if you do not want the date in these
     * documentation elements.
     *
     * @return a string representing the current timestamp, or null
     */
    @Override
    protected String getDateString() {
        if (suppressDate) {
            return null;
        } else {
            return LocalDateTime.now().format(dateFormat);
        }
    }

    @Override
    public void addModelClassComment(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        if (suppressAllComments || !addRemarkComments) {
            return;
        }

        topLevelClass.addJavaDocLine("/**");
        topLevelClass.addJavaDocLine(" * @author " + author);
        topLevelClass.addJavaDocLine(" * @date " + this.getDateString());
        topLevelClass.addJavaDocLine(" * <p>");
        topLevelClass.addJavaDocLine(" * " + introspectedTable.getRemarks() + "对应表:" + introspectedTable.getFullyQualifiedTable().getIntrospectedTableName());
        topLevelClass.addJavaDocLine(" */");
    }

    @Override
    public void addFieldComment(Field field, IntrospectedTable introspectedTable,
                                IntrospectedColumn introspectedColumn) {
        if (suppressAllComments) {
            return;
        }

        field.addJavaDocLine("/**");

        String remarks = introspectedColumn.getRemarks();
        if (addRemarkComments && StringUtility.stringHasValue(remarks)) {
            String[] remarkLines = remarks.split(System.getProperty("line.separator"));
            for (String remarkLine : remarkLines) {
                field.addJavaDocLine(" * " + remarkLine);
            }
        }

        addJavadocTag(field, false);

        field.addJavaDocLine(" */");
    }


    @Override
    public void addFieldComment(Field field, IntrospectedTable introspectedTable) {
        if (suppressAllComments) {
            return;
        }

        field.addJavaDocLine("/**");
        addJavadocTag(field, false);
        field.addJavaDocLine(" */");
    }
}
