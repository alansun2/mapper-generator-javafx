package ${PACKAGE};

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import vip.tuoyang.base.core.support.UserResourceHolder;
import vip.tuoyang.base.core.util.AssertUtils;
import ${package_prefix}.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DO;
import ${package_prefix}.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}GatewayI;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.convert.${TYPE_NAME_UPPER_CAMEL}DOConvertMapper;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.dataobject.${TYPE_NAME_UPPER_CAMEL};
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}Mapper;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static ${package_prefix}.<#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}DynamicSqlSupport.id;
import static ${package_prefix}.<#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}DynamicSqlSupport.isDelete;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Slf4j
@Component
public class ${TYPE_NAME_UPPER_CAMEL}GatewayImpl implements ${TYPE_NAME_UPPER_CAMEL}GatewayI {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    @Override
    public void save${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO) {
        ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = new ${TYPE_NAME_UPPER_CAMEL}();
        BeanUtils.copyProperties(${TYPE_NAME_LOWER_CAMEL}DO, ${TYPE_NAME_LOWER_CAMEL});
        ${TYPE_NAME_LOWER_CAMEL}.setCreateTime(LocalDateTime.now());
        ${TYPE_NAME_LOWER_CAMEL}.setCreateBy(UserResourceHolder.getUserNameAndId());

        final int ic = ${TYPE_NAME_LOWER_CAMEL}Mapper.insertSelective(${TYPE_NAME_LOWER_CAMEL});
        AssertUtils.isTrue(ic == 1, "添加异常，请稍后重试");
    }

    @Override
    public void save${TYPE_NAME_UPPER_CAMEL}Batch(List<${TYPE_NAME_UPPER_CAMEL}DO> ${TYPE_NAME_LOWER_CAMEL}DOS) {
        final LocalDateTime now = LocalDateTime.now();
        final String userNameAndId = UserResourceHolder.getUserNameAndId();
        final List<InspectionTaskConfigItemSnapshot> inspectionTaskConfigItemSnapshots = ${TYPE_NAME_LOWER_CAMEL}DOS.stream().map(item -> {
            ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = new ${TYPE_NAME_UPPER_CAMEL}();
            BeanUtils.copyProperties(${TYPE_NAME_LOWER_CAMEL}DO, ${TYPE_NAME_LOWER_CAMEL});
            ${TYPE_NAME_LOWER_CAMEL}.setIsDelete(BaseConstants.IS_DELETE_0);
            ${TYPE_NAME_LOWER_CAMEL}.setCreateTime(now);
            ${TYPE_NAME_LOWER_CAMEL}.setCreateBy(userNameAndId);
            return ${TYPE_NAME_LOWER_CAMEL};
        }).toList();

        final int ic = ${TYPE_NAME_LOWER_CAMEL}Mapper.insertMultiple(inspectionTaskConfigItemSnapshots);
        AssertUtils.isTrue(ic == ${TYPE_NAME_LOWER_CAMEL}DOS.size(), "添加异常，请稍后重试");
    }

    @Override
    public void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO) {
        ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = new ${TYPE_NAME_UPPER_CAMEL}();
        BeanUtils.copyProperties(${TYPE_NAME_LOWER_CAMEL}DO, ${TYPE_NAME_LOWER_CAMEL});
        ${TYPE_NAME_LOWER_CAMEL}.setUpdateTime(LocalDateTime.now());
        ${TYPE_NAME_LOWER_CAMEL}.setUpdateBy(UserResourceHolder.getUserNameAndId());
        final int uc = ${TYPE_NAME_LOWER_CAMEL}Mapper.updateByPrimaryKeySelective(${TYPE_NAME_LOWER_CAMEL});
        AssertUtils.isTrue(uc == 1, "更新异常: 记录可能已被删除，请刷新后重试");
    }

    @Override
    public Optional<${TYPE_NAME_UPPER_CAMEL}DO> get${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}> ${TYPE_NAME_LOWER_CAMEL}Opt = ${TYPE_NAME_LOWER_CAMEL}Mapper.selectByPrimaryKey(id);
        return ${TYPE_NAME_LOWER_CAMEL}Opt.map(${TYPE_NAME_UPPER_CAMEL}DOConvertMapper.INSTANCE::convertToDO);
    }

    @Override
    public List<${TYPE_NAME_UPPER_CAMEL}DO> get${TYPE_NAME_UPPER_CAMEL}ByIds(List<Long> ids, boolean isNeedDelete) {
            final ${TYPE_NAME_UPPER_CAMEL}DOConvertMapper instance = ${TYPE_NAME_UPPER_CAMEL}DOConvertMapper.INSTANCE;
            return ${TYPE_NAME_LOWER_CAMEL}Mapper.select(s -> s.where()
            .and(${TYPE_NAME_UPPER_CAMEL}DynamicSqlSupport.id, SqlBuilder.isIn(ids))
            .and(${TYPE_NAME_UPPER_CAMEL}DynamicSqlSupport.isDelete, SqlBuilder.isEqualTo(isNeedDelete).filter(aByte -> !aByte).map(aBoolean -> BaseConstants.IS_DELETE_0))
            ).stream().map(instance::convertToDO).toList();
    }
}