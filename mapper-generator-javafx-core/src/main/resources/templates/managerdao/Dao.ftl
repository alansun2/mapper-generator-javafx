<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import com.sy.common.bo.Query;
<#assign suffixs = ["", "Mapper"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Repository
public class ${TYPE_NAME_UPPER_CAMEL}Dao {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    public void insertSelective(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        ${TYPE_NAME_LOWER_CAMEL}Mapper.insertSelective(${TYPE_NAME_LOWER_CAMEL});
    }

    public List<${TYPE_NAME_UPPER_CAMEL}> queryList(final Query query) {
        return ${TYPE_NAME_LOWER_CAMEL}Mapper.selectList(query);
    }

    public Optional<${TYPE_NAME_UPPER_CAMEL}> queryById(final Long id) {
        return Optional.ofNullable(${TYPE_NAME_LOWER_CAMEL}Mapper.selectByPrimaryKey(id));
    }

    public int updateByIdSelective(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        return ${TYPE_NAME_LOWER_CAMEL}Mapper.updateByPrimaryKeySelective(${TYPE_NAME_LOWER_CAMEL});
    }

    public int deleteById(final Long id) {
        return ${TYPE_NAME_LOWER_CAMEL}Mapper.deleteByPrimaryKey(id);
    }
}