package ${PACKAGE};

import org.mybatis.dynamic.sql.SqlBuilder;
import org.mybatis.dynamic.sql.render.RenderingStrategies;
import org.mybatis.dynamic.sql.select.render.SelectStatementProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import vip.tuoyang.base.core.bean.request.PageRequest;
import vip.tuoyang.base.core.bean.response.Page;
import vip.tuoyang.schoolsafe.${server}.dto<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}PageQry;
import vip.tuoyang.schoolsafe.${server}.dto.data<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DTO;
import vip.tuoyang.schoolsafe.${server}<#if DOMAIN != "">.${DOMAIN}</#if>.dataobject.${TYPE_NAME_UPPER_CAMEL};
import vip.tuoyang.schoolsafe.${server}<#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}Mapper;

import java.util.stream.Collectors;

import static org.mybatis.dynamic.sql.SqlBuilder.isLikeWhenPresent;
import static vip.tuoyang.schoolsafe.${server}<#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}DynamicSqlSupport.*;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}PageQryExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    public Page<${TYPE_NAME_UPPER_CAMEL}DTO> execute(${TYPE_NAME_UPPER_CAMEL}PageQry qry) {
        return qry.doPage(() -> {
                    final SelectStatementProvider render = SqlBuilder.select().from(${TYPE_NAME_LOWER_CAMEL}).where()
                            .and(username, isLikeWhenPresent(qry.getUsername()).map(s -> PageRequest.getLike(qry.getUsername())))
                            .build().render(RenderingStrategies.MYBATIS3);
                    return ${TYPE_NAME_LOWER_CAMEL}Mapper.selectMany(render);
                },
                ${TYPE_NAME_LOWER_CAMEL}List -> ${TYPE_NAME_LOWER_CAMEL}List.stream().map(this::convert).collect(Collectors.toList()));

    }

    private ${TYPE_NAME_UPPER_CAMEL}DTO convert(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        ${TYPE_NAME_UPPER_CAMEL}DTO ${TYPE_NAME_LOWER_CAMEL}DTO = new ${TYPE_NAME_UPPER_CAMEL}DTO();
        <#list FIELDS_UPPER_CAMELS as item>
        ${TYPE_NAME_LOWER_CAMEL}DTO.set${item}(${TYPE_NAME_LOWER_CAMEL}.get${item}());
        </#list>
        return ${TYPE_NAME_LOWER_CAMEL}DTO;
    }
}