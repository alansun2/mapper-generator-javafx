package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import vip.tuoyang.base.core.bean.response.Page;
import ${package_prefix}.api<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}ServiceI;
import ${package_prefix}.dto.data<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DTO;
import ${package_prefix}.dto.data<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}PageDTO;
import ${package_prefix}.dto<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}Cmd;
import ${package_prefix}.dto<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}PageQry;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}AddCmdExe;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}DelByIdCmdExe;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}UpdateCmdExe;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.executor.query.${TYPE_NAME_UPPER_CAMEL}ByIdQryExe;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.executor.query.${TYPE_NAME_UPPER_CAMEL}PageQryExe;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}ServiceImpl implements ${TYPE_NAME_UPPER_CAMEL}ServiceI {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void add${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        final ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = this.convert(cmd);
        ${TYPE_NAME_LOWER_CAMEL}GatewayI.save${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_LOWER_CAMEL}DO);
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
        final Optional<${TYPE_NAME_UPPER_CAMEL}DO> ${TYPE_NAME_LOWER_CAMEL}DoOpt = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${TYPE_NAME_UPPER_CAMEL}ById(id);
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}DoOpt.isPresent() && ${TYPE_NAME_LOWER_CAMEL}DoOpt.get().getIsDelete() == BaseConstants.IS_DELETE_0,
                "记录不存在", HttpStatus.NOT_FOUND.value());
        return this.convert(${TYPE_NAME_LOWER_CAMEL}DoOpt.get());
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}DO> ${TYPE_NAME_LOWER_CAMEL}DoOpt = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${TYPE_NAME_UPPER_CAMEL}ById(cmd.getId());
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}DoOpt.isPresent() && ${TYPE_NAME_LOWER_CAMEL}DoOpt.get().getIsDelete() == BaseConstants.IS_DELETE_0,
        "记录不存在", HttpStatus.NOT_FOUND.value());
        final ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = this.convert(cmd);
        ${TYPE_NAME_LOWER_CAMEL}GatewayI.update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_LOWER_CAMEL}DO);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void delete${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}DO> byIdOpt = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${TYPE_NAME_UPPER_CAMEL}ById(id);
        if (byIdOpt.isPresent() && byIdOpt.get().getIsDelete() == BaseConstants.IS_DELETE_0) {
            ${TYPE_NAME_LOWER_CAMEL}Mapper.deleteById(id, UserResourceHolder.getUserNameAndId());
        }
    }

    private ${TYPE_NAME_UPPER_CAMEL}DO convert(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = new ${TYPE_NAME_UPPER_CAMEL}DO();
        <#list FIELDS_UPPER_CAMELS as item>
            ${TYPE_NAME_LOWER_CAMEL}DO.set${item}(cmd.get${item}());
        </#list>
        return ${TYPE_NAME_LOWER_CAMEL}DO;
    }

    private ${TYPE_NAME_UPPER_CAMEL}PageDTO convert(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        ${TYPE_NAME_UPPER_CAMEL}PageDTO ${TYPE_NAME_LOWER_CAMEL}PageDTO = new ${TYPE_NAME_UPPER_CAMEL}PageDTO();
        <#list FIELDS_UPPER_CAMELS as item>
        ${TYPE_NAME_LOWER_CAMEL}PageDTO.set${item}(${TYPE_NAME_LOWER_CAMEL}.get${item}());
        </#list>
        return ${TYPE_NAME_LOWER_CAMEL}PageDTO;
    }

    private ${TYPE_NAME_UPPER_CAMEL}DTO convert(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO) {
        ${TYPE_NAME_UPPER_CAMEL}DTO ${TYPE_NAME_LOWER_CAMEL}DTO = new ${TYPE_NAME_UPPER_CAMEL}DTO();
        <#list FIELDS_UPPER_CAMELS as item>
        ${TYPE_NAME_LOWER_CAMEL}DTO.set${item}(${TYPE_NAME_LOWER_CAMEL}DO.get${item}());
        </#list>
        return ${TYPE_NAME_LOWER_CAMEL}DTO;
    }
}