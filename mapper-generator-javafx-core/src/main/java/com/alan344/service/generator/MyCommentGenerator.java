package com.alan344.service.generator;

import org.mybatis.generator.api.CommentGenerator;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.InnerClass;
import org.mybatis.generator.api.dom.java.InnerEnum;
import org.mybatis.generator.api.dom.java.JavaElement;
import org.mybatis.generator.api.dom.java.Method;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.api.dom.xml.TextElement;
import org.mybatis.generator.api.dom.xml.XmlElement;
import org.mybatis.generator.config.MergeConstants;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.internal.util.StringUtility;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Properties;

/**
 * @author AlanSun
 * @date 2023/4/26 1:43
 * <p>
 * mybatis-generator 自定义注释生成器
 **/
public class MyCommentGenerator implements CommentGenerator {

    private final Properties properties = new Properties();

    private final boolean suppressDate;

    private final boolean suppressAllComments;

    /**
     * If suppressAllComments is true, this option is ignored.
     */
    private final boolean addRemarkComments;

    private static DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("yyyy-dd-MM HH:mm:ss");

    private String author;

    public MyCommentGenerator() {
        suppressDate = true;
        suppressAllComments = false;
        addRemarkComments = true;
    }

    @Override
    public void addConfigurationProperties(Properties props) {
        this.properties.putAll(props);

        author = properties.getProperty("author");

        String dateFormatString = properties.getProperty(PropertyRegistry.COMMENT_GENERATOR_DATE_FORMAT);
        if (StringUtility.stringHasValue(dateFormatString)) {
            dateFormat = DateTimeFormatter.ofPattern(dateFormatString);
        }
    }

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
        topLevelClass.addJavaDocLine(" * @date " + LocalDateTime.now().format(dateFormat));
        topLevelClass.addJavaDocLine(" * <p>");
        topLevelClass.addJavaDocLine(" * " + introspectedTable.getRemarks() + "对应表: " + introspectedTable.getFullyQualifiedTable().getIntrospectedTableName());
        topLevelClass.addJavaDocLine(" */");
    }

    @Override
    public void addFieldComment(Field field, IntrospectedTable introspectedTable,
                                IntrospectedColumn introspectedColumn) {
        if (suppressAllComments) {
            return;
        }

        field.addJavaDocLine("/**");

        if (addRemarkComments) {
            String remarks = introspectedColumn.getRemarks();
            if (StringUtility.stringHasValue(remarks)) {
                String[] remarkLines = remarks.split(System.lineSeparator());
                for (String remarkLine : remarkLines) {
                    field.addJavaDocLine(" * " + remarkLine);
                }
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

    @Override
    public void addComment(XmlElement xmlElement) {
        if (suppressAllComments) {
            return;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("<!-- ");
        sb.append(MergeConstants.NEW_ELEMENT_TAG);
        sb.append(" -->");
        xmlElement.addElement(new TextElement(sb.toString()));

        String s = getDateString();
        if (s != null) {
            sb.setLength(0);
            sb.append("  This element was generated on ");
            sb.append(s);
            sb.append('.');
            xmlElement.addElement(new TextElement(sb.toString()));
        }
    }

    @Override
    public void addClassComment(InnerClass innerClass, IntrospectedTable introspectedTable) {
        if (suppressAllComments) {
            return;
        }

        innerClass.addJavaDocLine("/**");

        addJavadocTag(innerClass, false);

        innerClass.addJavaDocLine(" */");
    }

    @Override
    public void addClassComment(InnerClass innerClass, IntrospectedTable introspectedTable, boolean markAsDoNotDelete) {
        if (suppressAllComments) {
            return;
        }

        innerClass.addJavaDocLine("/**");

        addJavadocTag(innerClass, markAsDoNotDelete);

        innerClass.addJavaDocLine(" */");
    }


    @Override
    public void addEnumComment(InnerEnum innerEnum, IntrospectedTable introspectedTable) {
        if (suppressAllComments) {
            return;
        }

        innerEnum.addJavaDocLine("/**");

        addJavadocTag(innerEnum, false);

        innerEnum.addJavaDocLine(" */");
    }

    @Override
    public void addGeneralMethodComment(Method method, IntrospectedTable introspectedTable) {
        if (suppressAllComments) {
            return;
        }

        method.addJavaDocLine("/**");

        addJavadocTag(method, false);

        method.addJavaDocLine(" */");
    }

    @Override
    public void addGetterComment(Method method, IntrospectedTable introspectedTable,
                                 IntrospectedColumn introspectedColumn) {
        if (suppressAllComments) {
            return;
        }

        method.addJavaDocLine("/**");

        addJavadocTag(method, false);

        method.addJavaDocLine(" */");
    }

    @Override
    public void addSetterComment(Method method, IntrospectedTable introspectedTable,
                                 IntrospectedColumn introspectedColumn) {
        if (suppressAllComments) {
            return;
        }

        method.addJavaDocLine("/**");

        addJavadocTag(method, false);

        method.addJavaDocLine(" */");
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
        final String lastLine = javaElement.getJavaDocLines().get(javaElement.getJavaDocLines().size() - 1);
        if (!"/**".equalsIgnoreCase(lastLine)) {
            javaElement.addJavaDocLine(" *");
        }
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
}
