package com.alan344.plugin;

import com.alan344.bean.MybatisConfigThreadLocal;
import com.alan344.bean.MybatisExportConfig;
import com.alan344.bean.ServiceConfig;
import com.alan344.bean.ServiceConfigThreadLocal;
import com.alan344.utils.TableUtils;
import com.alan344happyframework.constants.SeparatorConstants;
import com.alan344happyframework.util.DateUtils;
import com.github.javaparser.JavaParser;
import org.apache.commons.lang3.StringUtils;
import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.*;
import org.mybatis.generator.codegen.RootClassInfo;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.internal.util.StringUtility;

import java.util.Collections;
import java.util.List;
import java.util.Properties;

/**
 * @author AlanSun
 * @date 2020/12/3 14:19
 */
public class DefaultDynamicGeneratorPlugin extends PluginAdapter {

    private boolean rest;
    private String targetProject;
    private String targetPackage;
    private String respond;

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    public void setProperties(Properties properties) {
        super.setProperties(properties);
        this.rest = StringUtility.isTrue(properties.getProperty("rest"));
        this.targetProject = properties.getProperty("targetProject");
        this.targetPackage = properties.getProperty("targetPackage");
        this.respond = properties.getProperty("respond");
    }

    @Override
    public List<GeneratedJavaFile> contextGenerateAdditionalJavaFiles(IntrospectedTable introspectedTable) {
        JavaParser javaParser = new JavaParser();

        final ServiceConfig serviceConfig = ServiceConfigThreadLocal.getServiceConfig();
        final String servicePackage = serviceConfig.getServicePackage();
        if (StringUtils.isBlank(servicePackage)) {
            return Collections.emptyList();
        }

        // 生成类名
        final String baseRecordType = introspectedTable.getBaseRecordType();
        String servicePackageFull = serviceConfig.getServicePackage() + SeparatorConstants.DOT + baseRecordType.substring(baseRecordType.lastIndexOf(SeparatorConstants.DOT) + 1) + "Service";
        FullyQualifiedJavaType type = new FullyQualifiedJavaType(servicePackageFull);
        TopLevelClass topLevelClass = new TopLevelClass(type);
        topLevelClass.setVisibility(JavaVisibility.PUBLIC);
        // 添加 import
        this.addImport(topLevelClass, introspectedTable, serviceConfig);
        // 添加 spring @Service 注解
        this.addSpringServiceAnnotation(topLevelClass);
        // 添加类注释
        this.addClassComment(topLevelClass);
        // 添加方法
        this.addPackageMethod(topLevelClass, introspectedTable, serviceConfig);
        this.addInsertMethod(topLevelClass, introspectedTable, serviceConfig);

        GeneratedJavaFile generatedJavaFile = new GeneratedJavaFile(topLevelClass, context.getJavaModelGeneratorConfiguration().getTargetProject(), context.getProperty(PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING), context.getJavaFormatter());
        return Collections.singletonList(generatedJavaFile);
    }

    private void addImport(TopLevelClass topLevelClass, IntrospectedTable introspectedTable, ServiceConfig serviceConfig) {
        final FullyQualifiedJavaType requestBean = TableUtils.getRequestBeanType(introspectedTable, serviceConfig);
        topLevelClass.addImportedType(requestBean);
        String originalBeanName = TableUtils.getOriginalBeanName(introspectedTable);
        MybatisExportConfig mybatisExportConfig = MybatisConfigThreadLocal.getMybatisExportConfig();
        topLevelClass.addImportedType(mybatisExportConfig.getMapperPackage() + SeparatorConstants.DOT + originalBeanName + "Mapper");
        topLevelClass.addImportedType("org.springframework.util.Assert");
        topLevelClass.addImportedType("cn.com.asoco.base.support.UserResourceHolderSynchronization");
        topLevelClass.addImportedType("org.springframework.transaction.annotation.Transactional");
        topLevelClass.addImportedType("java.time.LocalDateTime");
        topLevelClass.addImportedType("java.util.List");
        topLevelClass.addImportedType(mybatisExportConfig.getBeanPackage() + SeparatorConstants.DOT + originalBeanName);
    }

    /**
     * 添加 spring @Service 注解
     *
     * @param topLevelClass 类
     */
    private void addSpringServiceAnnotation(TopLevelClass topLevelClass) {
        topLevelClass.addImportedType("org.springframework.stereotype.Service");
        topLevelClass.addAnnotation("@Service");
    }

    /**
     * 添加类注释
     *
     * @param topLevelClass 类
     */
    private void addClassComment(TopLevelClass topLevelClass) {
        //添加类注释
        topLevelClass.addJavaDocLine("/**");
        topLevelClass.addJavaDocLine(" * @author " + MybatisConfigThreadLocal.getMybatisExportConfig().getAuthor());
        //添加时间
        topLevelClass.addJavaDocLine(" * @date " + DateUtils.getCurrentDate());
        topLevelClass.addJavaDocLine(" */");
    }

    /**
     * 添加 package 方法
     */
    private void addPackageMethod(TopLevelClass topLevelClass, IntrospectedTable introspectedTable, ServiceConfig serviceConfig) {
        String originalBeanName = TableUtils.getOriginalBeanName(introspectedTable);
        final String request = "request";
        String record = "record";
        final FullyQualifiedJavaType requestBean = TableUtils.getRequestBeanType(introspectedTable, serviceConfig);

        Method method = new Method("package" + originalBeanName);
        method.setVisibility(JavaVisibility.PUBLIC);
        method.setReturnType(topLevelClass.getType());
        method.addParameter(new Parameter(new FullyQualifiedJavaType(requestBean.getShortName()), request));

        method.addBodyLine(originalBeanName + " " + record + " = new " + originalBeanName + "();");
        final List<IntrospectedColumn> nonPrimaryKeyColumns = introspectedTable.getNonPrimaryKeyColumns();
        String rootClass = TableUtils.getRootClass(introspectedTable, context);
        for (IntrospectedColumn introspectedColumn : nonPrimaryKeyColumns) {
            if (RootClassInfo.getInstance(rootClass, Collections.emptyList()).containsProperty(introspectedColumn)) {
                continue;
            }

            // 全局忽略字段
            if (TableUtils.isFieldIgnore(serviceConfig.getRequestGlobalIgnoreColumns(), introspectedColumn.getActualColumnName())) {
                continue;
            }

            method.addBodyLine(record + ".set" + originalBeanName + "(" + request + ".get" + originalBeanName + "())");
        }
        method.addBodyLine("return " + record);

        topLevelClass.addMethod(method);
    }

    private void addInsertMethod(TopLevelClass topLevelClass, IntrospectedTable introspectedTable, ServiceConfig serviceConfig) {
        String originalBeanName = TableUtils.getOriginalBeanName(introspectedTable);
        final String request = "request";
        final FullyQualifiedJavaType requestBean = TableUtils.getRequestBeanType(introspectedTable, serviceConfig);

        Method method = new Method("add" + originalBeanName);
        method.setVisibility(JavaVisibility.PUBLIC);
        topLevelClass.addImportedType(requestBean);
        method.addParameter(new Parameter(new FullyQualifiedJavaType(requestBean.getShortName()), request));


        method.addJavaDocLine("/**");
        method.addJavaDocLine(" * 新增");
        method.addJavaDocLine(" *");
        method.addJavaDocLine(" * @param request {@link " + requestBean.getShortName() + "}");
        method.addJavaDocLine(" * @return {@link " + originalBeanName + "}");
        method.addJavaDocLine(" */");

        method.addAnnotation("@Transactional(rollbackFor = Exception.class)");

        method.addBodyLine("final " + originalBeanName + " record = package" + originalBeanName + "(request);");
        method.addBodyLine("record.setCreateBy(UserResourceHolderSynchronization.getUserName());");
        method.addBodyLine("record.setCreateTime(LocalDateTime.now());");
        method.addBodyLine("final int ic = " + TableUtils.firstLetterDownCase(originalBeanName) + "Mapper.insertSelective(record);");
        method.addBodyLine("Assert.isTrue(ic == 1, \"新增失败\");");

        topLevelClass.addMethod(method);
    }
}