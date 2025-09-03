<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

<#assign suffixs = ["ServiceI"]>
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.xxxx.base.core.bean.response.Page;
<#assign suffixs = ["", "DTO", "PageDTO", "Cmd", "PageQry", "Mapper"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

import java.util.Optional;
/**
 * @author ${author}
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}ServiceImpl implements ${TYPE_NAME_UPPER_CAMEL}ServiceI {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void add${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = this.convert(cmd);
        ${TYPE_NAME_LOWER_CAMEL}Mapper.insertSelective(${TYPE_NAME_LOWER_CAMEL});
    }

    @Override
    public Page<${TYPE_NAME_UPPER_CAMEL}PageDTO> get${TYPE_NAME_UPPER_CAMEL}Page(${TYPE_NAME_UPPER_CAMEL}PageQry qry) {
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

    @Override
    public ${TYPE_NAME_UPPER_CAMEL}DTO get${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}> ${TYPE_NAME_LOWER_CAMEL}Opt = ${TYPE_NAME_LOWER_CAMEL}Mapper.selectByPrimaryKey(id);
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}Opt.isPresent() && ${TYPE_NAME_LOWER_CAMEL}Opt.get().getIsDelete() == BaseConstants.IS_DELETE_0,
                "记录不存在", HttpStatus.NOT_FOUND.value());
        return this.convertToDTO(${TYPE_NAME_LOWER_CAMEL}Opt.get());
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}> ${TYPE_NAME_LOWER_CAMEL}Opt = ${TYPE_NAME_LOWER_CAMEL}Mapper.selectByPrimaryKey(cmd.getId());
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}Opt.isPresent() && ${TYPE_NAME_LOWER_CAMEL}Opt.get().getIsDelete() == BaseConstants.IS_DELETE_0,
            "记录不存在", HttpStatus.NOT_FOUND.value());
        final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = this.convert(cmd);
        ${TYPE_NAME_LOWER_CAMEL}Mapper.updateByPrimaryKeySelective(${TYPE_NAME_LOWER_CAMEL});
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void delete${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
    final Optional<${TYPE_NAME_UPPER_CAMEL}> byIdOpt = ${TYPE_NAME_LOWER_CAMEL}Mapper.selectByPrimaryKey(id);
        if (byIdOpt.isPresent() && byIdOpt.get().getIsDelete() == BaseConstants.IS_DELETE_0) {
            ${TYPE_NAME_LOWER_CAMEL}Mapper.deleteByPrimaryKey(id);
        }
    }

    private ${TYPE_NAME_UPPER_CAMEL} convert(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}" "${TYPE_NAME_LOWER_CAMEL}" "cmd" "null" "Cmd" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP!/>
        return ${TYPE_NAME_LOWER_CAMEL};
    }

    private ${TYPE_NAME_UPPER_CAMEL}PageDTO convert(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}PageDTO" "${TYPE_NAME_LOWER_CAMEL}PageDTO" "${TYPE_NAME_LOWER_CAMEL}" "PageDTO" "null" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP!/>
        return ${TYPE_NAME_LOWER_CAMEL}PageDTO;
    }

    private ${TYPE_NAME_UPPER_CAMEL}DTO convertToDTO(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}DTO" "${TYPE_NAME_LOWER_CAMEL}DTO" "${TYPE_NAME_LOWER_CAMEL}" "DTO" "DO" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP!/>
        return ${TYPE_NAME_LOWER_CAMEL}DTO;
    }
}