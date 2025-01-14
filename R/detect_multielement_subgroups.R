#' @title Detect Multi-Element Subgroups
#'
#' @description
#' Detect subgroups inside groups and give them a subgroup index case they have more than 1 element.
#'
#' @param subgroup_property Vector (numeric or character) with a secondary group property
#' @param groups_midx Numeric vector with group indexes indicating instaces to be compared against each other.
#'
#' @return Numeric vector with indexes for subgroups with the same subroup property.
#' @export
#'
#' @examples
#' detect_multielement_subgroups(c(45, 5, 33, 657, 45, 67), c(1 ,0 ,0 ,0 ,1 ,1))

detect_multielement_subgroups = function(subgroup_property, groups_midx){

  #..initialize output variable..
  tot_instances = length(subgroup_property)
  subgroup_midx = rep(0, tot_instances)

  #..for each group, flag all instances which also have the same subgroup property..
  c_idx = 1#..initialize subgroup index for multi-copy candidates..
  tot_group_midx = max(groups_midx)

  #..initialize progress bar..
  pb = get_progressbar(tot_group_midx)

  #..check if subgroup_property of group elements match..
  for(i in 1:tot_group_midx){

    #..separate data for the current group..
    pos_flag = groups_midx%in%i#..flag group elementss..
    subgroup_property_multi = subgroup_property[pos_flag]#..get elements subgroup_property..

    #..initialize variables for current block of data..
    tot_block = length(subgroup_property_multi)
    c_subgroup_midx = rep(0, tot_block)

    #..check which elements (for the current group id) have the same size..
    for(i2 in 1:tot_block){
      if(c_subgroup_midx[i2]==0){#..entry not analyzed yet..
        data_subgroup_property_flag = subgroup_property_multi%in%subgroup_property_multi[i2]#..check which entries have same size as current entry..
        if(sum(data_subgroup_property_flag)>1){#..case multiple instances with same subgroup property are detected..
          c_subgroup_midx[data_subgroup_property_flag] = c_idx#..give them a index..
          c_idx = c_idx + 1#..update index
        }
      }
    }
    subgroup_midx[pos_flag] = c_subgroup_midx#..join results from current block of data to the full set of results..

   pb$tick()#..update progress bar..
  }

  return(subgroup_midx)
}
