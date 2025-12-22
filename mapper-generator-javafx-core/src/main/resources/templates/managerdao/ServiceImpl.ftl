<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import com.sy.common.bo.Page;
import com.sy.common.bo.Query;
<#assign suffixs = ["Dao", "DTO", "PageDTO", "", "Manager", "Service", "PageVO", "VO"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import com.sysafari.common.core.utils.AssertUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}ServiceImpl implements ${TYPE_NAME_UPPER_CAMEL}Service {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Manager ${TYPE_NAME_LOWER_CAMEL}Manager;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Dao ${TYPE_NAME_LOWER_CAMEL}Dao;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void add${TYPE_NAME_UPPER_CAMEL}(final ${TYPE_NAME_UPPER_CAMEL}DTO dto) {
        final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = ${TYPE_NAME_LOWER_CAMEL}Manager.convert(dto);
        ${TYPE_NAME_LOWER_CAMEL}Dao.insertSelective(${TYPE_NAME_LOWER_CAMEL});
    }

    @Override
    public Page<${TYPE_NAME_UPPER_CAMEL}PageVO> list${TYPE_NAME_UPPER_CAMEL}(final ${TYPE_NAME_UPPER_CAMEL}PageDTO dto) {
        return dto.doPage(() -> ${TYPE_NAME_LOWER_CAMEL}Dao.queryList(Query.create(dto)), ${TYPE_NAME_LOWER_CAMEL}List ->
                ${TYPE_NAME_LOWER_CAMEL}List.stream().map(${TYPE_NAME_LOWER_CAMEL}Manager::convert2PageVO).collect(Collectors.toList()));
    }

    @Override
    public ${TYPE_NAME_UPPER_CAMEL}VO get${TYPE_NAME_UPPER_CAMEL}ById(final Long id) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}> ${TYPE_NAME_LOWER_CAMEL}Opt = ${TYPE_NAME_LOWER_CAMEL}Dao.queryById(id);
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}Opt.isPresent() && !${TYPE_NAME_LOWER_CAMEL}Opt.get().getDeleted(),
                "记录不存在", HttpStatus.NOT_FOUND.value());
        return ${TYPE_NAME_LOWER_CAMEL}Manager.convert2VO(${TYPE_NAME_LOWER_CAMEL}Opt.get());
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void update${TYPE_NAME_UPPER_CAMEL}ById(final Long id,final ${TYPE_NAME_UPPER_CAMEL}DTO dto) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}> ${TYPE_NAME_LOWER_CAMEL}Opt = ${TYPE_NAME_LOWER_CAMEL}Dao.queryById(dto.getId());
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}Opt.isPresent() && !${TYPE_NAME_LOWER_CAMEL}Opt.get().getDeleted(),
                "记录不存在", HttpStatus.NOT_FOUND.value());
        final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = ${TYPE_NAME_LOWER_CAMEL}Manager.convert(dto);
        ${TYPE_NAME_LOWER_CAMEL}.setId(id);
        ${TYPE_NAME_LOWER_CAMEL}Dao.updateByIdSelective(${TYPE_NAME_LOWER_CAMEL});
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void delete${TYPE_NAME_UPPER_CAMEL}ById(final Long id) {
    final Optional<${TYPE_NAME_UPPER_CAMEL}> byIdOpt = ${TYPE_NAME_LOWER_CAMEL}Dao.queryById(id);
        if (byIdOpt.isPresent() && !byIdOpt.get().getDeleted()) {
            ${TYPE_NAME_LOWER_CAMEL}Dao.deleteById(id);
        }
    }
}