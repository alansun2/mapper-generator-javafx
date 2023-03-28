package com.alan344.plugin;

import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.Interface;
import org.mybatis.generator.api.dom.java.Method;
import org.mybatis.generator.api.dom.java.Parameter;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * @author AlanSun
 * @date 2022/11/8 8:52
 */
public class DeleteByIMethodPlugin extends PluginAdapter {
    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public boolean clientGenerated(Interface interfaze, IntrospectedTable introspectedTable) {
        final String targetRuntime = introspectedTable.getContext().getTargetRuntime();
        if (Objects.equals(targetRuntime, "MyBatis3DynamicSql")) {
            return this.dynamic(interfaze, introspectedTable);
        }
        return true;
    }

    public boolean dynamic(Interface interfaze, IntrospectedTable introspectedTable) {
        interfaze.addImportedType(new FullyQualifiedJavaType("java.time.LocalDateTime"));
        interfaze.addImportedType(new FullyQualifiedJavaType("vip.tuoyang.base.core.constants.BaseConstants"));
        interfaze.addStaticImport("org.mybatis.dynamic.sql.SqlBuilder.isIn");
        final List<Method> methods = interfaze.getMethods();
        final Optional<Method> first = methods.stream().filter(method -> "selectByPrimaryKey".equals(method.getName())).findFirst();
        final Method selectByPrimaryKeyMethod = first.orElseThrow();
        final Parameter parameter = selectByPrimaryKeyMethod.getParameters().get(0);

        Method deleteByIdMethod = new Method("deleteById");
        deleteByIdMethod.addJavaDocLine("/**");
        deleteByIdMethod.addJavaDocLine(" * 根据 id 删除");
        deleteByIdMethod.addJavaDocLine(" *");
        deleteByIdMethod.addJavaDocLine(" * @param id_       id");
        deleteByIdMethod.addJavaDocLine(" * @param updateBy_ 操作人");
        deleteByIdMethod.addJavaDocLine(" * @return 影响行数");
        deleteByIdMethod.addJavaDocLine(" */");
        deleteByIdMethod.setDefault(true);
        deleteByIdMethod.setReturnType(FullyQualifiedJavaType.getIntInstance());
        deleteByIdMethod.addParameter(0, new Parameter(parameter.getType(), "id_"));
        deleteByIdMethod.addParameter(1, new Parameter(FullyQualifiedJavaType.getStringInstance(), "updateBy_"));
        deleteByIdMethod.addBodyLine("return update(u -> u.set(isDelete).equalTo(BaseConstants.IS_DELETE_1)");
        deleteByIdMethod.addBodyLine("        .set(updateBy).equalTo(updateBy_)");
        deleteByIdMethod.addBodyLine("        .set(updateTime).equalTo(LocalDateTime.now())");
        deleteByIdMethod.addBodyLine("        .where()");
        deleteByIdMethod.addBodyLine("        .and(id, isEqualTo(id_))");
        deleteByIdMethod.addBodyLine("        .and(isDelete, isEqualTo(BaseConstants.IS_DELETE_0)));");
        interfaze.addMethod(deleteByIdMethod);

        final FullyQualifiedJavaType type = parameter.getType();
        Method deleteByIdsMethod = new Method("deleteByIds");
        deleteByIdsMethod.addJavaDocLine("/**");
        deleteByIdsMethod.addJavaDocLine(" * 根据 ids 删除");
        deleteByIdsMethod.addJavaDocLine(" *");
        deleteByIdsMethod.addJavaDocLine(" * @param ids       ids");
        deleteByIdsMethod.addJavaDocLine(" * @param updateBy_ 操作人");
        deleteByIdsMethod.addJavaDocLine(" * @return 影响行数");
        deleteByIdsMethod.addJavaDocLine(" */");
        deleteByIdsMethod.setDefault(true);
        deleteByIdsMethod.setReturnType(FullyQualifiedJavaType.getIntInstance());
        deleteByIdsMethod.addParameter(0, new Parameter(new FullyQualifiedJavaType("java.util.Collection<" + type.getShortName() + ">"), "ids"));
        deleteByIdsMethod.addParameter(1, new Parameter(FullyQualifiedJavaType.getStringInstance(), "updateBy_"));
        deleteByIdsMethod.addBodyLine("return update(u -> u.set(isDelete).equalTo(BaseConstants.IS_DELETE_1)");
        deleteByIdsMethod.addBodyLine("        .set(updateBy).equalTo(updateBy_)");
        deleteByIdsMethod.addBodyLine("        .set(updateTime).equalTo(LocalDateTime.now())");
        deleteByIdsMethod.addBodyLine("        .where()");
        deleteByIdsMethod.addBodyLine("        .and(id, isIn(ids))");
        deleteByIdsMethod.addBodyLine("        .and(isDelete, isEqualTo(BaseConstants.IS_DELETE_0)));");
        interfaze.addMethod(deleteByIdsMethod);
        return true;
    }
}
