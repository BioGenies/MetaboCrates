create_message_box <- function(text, type = "warning"){
  type_elements <- switch(type,
                          "description" = c("#effffb", "#11b78c", "&#9432"),
                          "warning" = c("#ffebeb","#e13535", "&#9888"))
  
  HTML(paste0(
    '<div style="text-align: left; background-color:',
    type_elements[1],
    ';
     padding: 12px 15px; border-left: 4px solid',
    type_elements[2],
    ';
     border-radius: 4px; font-size: 16px; width: 100%; color:',
    type_elements[2],
    '">
    <span style="margin-right: 12px; color:',
    type_elements[2],
    '; font-size: 23px;">',
    type_elements[3],
    ';</span>',
    text,
    '</div>'
  )
  )
}