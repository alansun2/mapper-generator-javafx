package ${PACKAGE};

import org.mybatis.dynamic.sql.SqlBuilder;
import org.mybatis.dynamic.sql.render.RenderingStrategies;
import org.mybatis.dynamic.sql.select.render.SelectStatementProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import vip.tuoyang.base.core.bean.request.PageRequest;
import vip.tuoyang.base.core.bean.response.Page;
import ${package_prefix}.dto<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}PageQry;
import ${package_prefix}.dto.data<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}PageDTO;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.dataobject.${TYPE_NAME_UPPER_CAMEL};
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}Mapper;

import java.util.stream.Collectors;

import static org.mybatis.dynamic.sql.SqlBuilder.isLikeWhenPresent;
import static ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}DynamicSqlSupport.*;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}PageQryExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    public Page<${TYPE_NAME_UPPER_CAMEL}PageDTO> execute(${TYPE_NAME_UPPER_CAMEL}PageQry qry) {
        return qry.doPage(() -> {
                    final SelectStatementProvider render = SqlBuilder.select(${TYPE_NAME_UPPER_CAMEL}Mapper.selectList)
                            .from(${TYPE_NAME_LOWER_CAMEL})
                            .where()
                            .and(username, isLikeWhenPresent(qry.getUsername()).map(s -> PageRequest.getLike(qry.getUsername())))
                            .build().render(RenderingStrategies.MYBATIS3);
                    return ${TYPE_NAME_LOWER_CAMEL}Mapper.selectMany(render);
                },
                ${TYPE_NAME_LOWER_CAMEL}List -> ${TYPE_NAME_LOWER_CAMEL}List.stream().map(this::convert).collect(Collectors.toList()));

    }

    private ${TYPE_NAME_UPPER_CAMEL}PageDTO convert(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        ${TYPE_NAME_UPPER_CAMEL}PageDTO ${TYPE_NAME_LOWER_CAMEL}PageDTO = new ${TYPE_NAME_UPPER_CAMEL}PageDTO();
        <#list FIELDS_UPPER_CAMELS as item>
        ${TYPE_NAME_LOWER_CAMEL}PageDTO.set${item}(${TYPE_NAME_LOWER_CAMEL}.get${item}());
        </#list>
        return ${TYPE_NAME_LOWER_CAMEL}PageDTO;
    }
}