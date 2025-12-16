<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.mybatis.dynamic.sql.SqlBuilder;
import org.mybatis.dynamic.sql.render.RenderingStrategies;
import org.mybatis.dynamic.sql.select.render.SelectStatementProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.xxxx.base.core.bean.request.PageRequest;
import com.xxxx.base.core.bean.response.Page;
<#assign suffixs = ["PageQry", "PageDTO", "", "Mapper"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

import java.util.stream.Collectors;

import static org.mybatis.dynamic.sql.SqlBuilder.isLikeWhenPresent;
import static <#if package_prefix??>${package_prefix}</#if><#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}DynamicSqlSupport.*;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
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
                            // .and(username, isLikeWhenPresent(qry.getUsername()).map(s -> PageRequest.getLike(qry.getUsername())))
                            .build().render(RenderingStrategies.MYBATIS3);
                    return ${TYPE_NAME_LOWER_CAMEL}Mapper.selectMany(render);
                },
                ${TYPE_NAME_LOWER_CAMEL}List -> ${TYPE_NAME_LOWER_CAMEL}List.stream().map(this::convert).collect(Collectors.toList()));

    }

    private ${TYPE_NAME_UPPER_CAMEL}PageDTO convert(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}PageDTO" "${TYPE_NAME_LOWER_CAMEL}PageDTO" "${TYPE_NAME_LOWER_CAMEL}" "PageDTO" "null" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP!/>
        return ${TYPE_NAME_LOWER_CAMEL}PageDTO;
    }
}