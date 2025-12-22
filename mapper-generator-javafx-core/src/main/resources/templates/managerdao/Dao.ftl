<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import com.sy.common.bo.Query;
<#assign suffixs = ["", "Mapper"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import com.sysafari.common.core.utils.AssertUtils;
import com.sysafari.common.snowflakegen.SnowflakeIdGenerator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import tk.mybatis.mapper.entity.Example;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Repository
public class ${TYPE_NAME_UPPER_CAMEL}Dao {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;
    @Autowired
    private SnowflakeIdGenerator snowflakeIdGenerator;

    public void insertSelective(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        ${TYPE_NAME_LOWER_CAMEL}.setId(snowflakeIdGenerator.nextId());
        final int c = ${TYPE_NAME_LOWER_CAMEL}Mapper.insertSelective(${TYPE_NAME_LOWER_CAMEL});
        AssertUtils.isTrue(c > 0, "插入失败");
    }

    public List<${TYPE_NAME_UPPER_CAMEL}> queryList(final Query query) {
        return ${TYPE_NAME_LOWER_CAMEL}Mapper.selectList(query);
    }

    public Optional<${TYPE_NAME_UPPER_CAMEL}> queryById(final Long id) {
        final Example example = new Example(${TYPE_NAME_UPPER_CAMEL}.class);
        example.createCriteria().andEqualTo("id", id).andEqualTo("deleted", false);
        return Optional.ofNullable(${TYPE_NAME_LOWER_CAMEL}Mapper.selectOneByExample(example));
    }

    public void updateByIdSelective(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        final Example example = new Example(${TYPE_NAME_UPPER_CAMEL}.class);
        example.createCriteria().andEqualTo("id", ${TYPE_NAME_LOWER_CAMEL}.getId()).andEqualTo("deleted", false);
        final int c = ${TYPE_NAME_LOWER_CAMEL}Mapper.updateByExampleSelective(${TYPE_NAME_LOWER_CAMEL}, example);
        AssertUtils.isTrue(c > 0, "更新失败, 可能数据已删除");
    }

    public void deleteById(final Long id) {
        final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL} = new ${TYPE_NAME_UPPER_CAMEL}();
        ${TYPE_NAME_LOWER_CAMEL}.setId(id);
        ${TYPE_NAME_LOWER_CAMEL}.setDeleted(true);
        final Example example = new Example(${TYPE_NAME_UPPER_CAMEL}.class);
        example.createCriteria().andEqualTo("id", id).andEqualTo("deleted", false);
        final int c = ${TYPE_NAME_LOWER_CAMEL}Mapper.updateByExampleSelective(${TYPE_NAME_LOWER_CAMEL}, example);
        AssertUtils.isTrue(c <= 1, "删除失败, 可能数据已删除");
    }
}